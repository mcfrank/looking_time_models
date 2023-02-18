import os
import torch
import numpy as np
import torchvision
import PIL.Image as Image
import torchvision.models as models
import json

# %% Model definition

class PspaceModel(torch.nn.Module):

    def __init__(self,  device:str):
        super().__init__()

        # Load perceptually-aligned linear projection weights
        _loc = os.path.dirname(__file__)
        x = torch.load(os.path.join(_loc, 'weights/checkpoint_step50000.pth'), map_location=device)
        w = x['model']['linear0.weight']
        logscaling = x['model']['param_logscaling']
        gamma = torch.exp(logscaling) # gamma is the scaling factor
        in_features = w.shape[1]
        out_features = w.shape[0]
        self.mapper = torch.nn.Linear(in_features = in_features, out_features = out_features, bias = False, device=device)
        self.mapper.weight = torch.nn.Parameter(w* gamma)

        # Register hook for getting intermediate feature output from a base model
        self.base_model = models.resnet50(pretrained=True,).to(device).train(mode = False)
        intermediate_layer = 'avgpool'
        self._current_values= {}
        f = getattr(self.base_model, intermediate_layer)
        f.register_forward_hook(self._register_hook(layer_name=intermediate_layer))

    def _register_hook(self, layer_name):
        def hook(module, input, intermediate_feature_output):
            self._current_values[layer_name] = intermediate_feature_output

        return hook

    def forward(self, x:torch.Tensor):
        """

        :param x: torch.Tensor of shape (batch_size, 3, 224, 224). Assumes standard Imagenet normalization of images. Can use load_images_to_tensor() to load images with this normalization tensor.
        :return:
        """
        self.base_model.forward(x)
        intermediate_feature_output = self._current_values['avgpool']
        intermediate_feature_output = intermediate_feature_output.reshape(intermediate_feature_output.shape[0], -1)
        # print(intermediate_feature_output)
        mapped_features = self.mapper(intermediate_feature_output)
        return mapped_features


# %% Image loading function
def load_images_to_tensor(image_paths:[str], img_size:tuple=(224, 224)):

    """
    Loads images at the image_paths to a torch.Tensor, applying standard torchvision preprocessing.

    The preprocessing steps are:
        0) Resize the image using bilinear interpolation to img_size.
        1) Convert the image from the [0-255] uint8 representation to the [0, 1] float representation
        2) Subtract the Imagenet channel means [0.485, 0.456, 0.406]
        3) Normalize by the Imagenet channel standard deviations [0.229, 0.224, 0.225]
        4) Reorder the dimensions from HCW order to CHW order

    Returns [len(image_paths), 3, H, W]

    :param image_paths:
    :param img_size: a tuple of length two
    :return: torch.Tensor
    :rtype:
    """

    # check whether it's a directory 
    if isinstance(image_paths, list):
        assert np.all([isinstance(im, str) for im in image_paths])
        assert isinstance(img_size, tuple)
        assert len(img_size) == 2

        # Return a torch.tensor of shape [nimages, c, h, w]
        pil_images = [Image.open(p).convert("RGB") for p in image_paths]
        image_names = image_paths
    
    else: #or a list of images
        images = os.listdir(image_paths)
        pil_images = []
        image_names = []
        for image in images:
            if '.png' in image:
                pil_images.append(Image.open(image_paths + image).convert("RGB")) 
                image_names.append(image)

    torchvision_imagenet_mean_normalization_transform = torchvision.transforms.Normalize(
        mean=[0.485, 0.456, 0.406],
        std=[0.229, 0.224, 0.225],
        inplace=True)


    standard_transform_stack = [
                torchvision.transforms.Resize(img_size, interpolation=torchvision.transforms.functional.InterpolationMode.BILINEAR),
                torchvision.transforms.ToTensor(), # Converts np.ndarray [h,w,c] or PIL.Image to [c,h,w] torch.Tensor, normalizing between 0 and 1  if the PIL Image is one of (L, LA, P, I, F, RGB, YCbCr, RGBA, CMYK, 1) or if the ndarray has dtype = np.uint8. Otherwise, no rescaling.
                torchvision_imagenet_mean_normalization_transform,
            ]

    transform_stack = standard_transform_stack
    transform = torchvision.transforms.Compose(transform_stack)

    images = torch.stack([transform(img) for img in pil_images], dim=0)
    return images, image_names


if __name__ == '__main__':
    # Example usage
    model = PspaceModel(device = 'cpu')
    image_paths = 'stims_smaller_set/padded/'
    X, image_names = load_images_to_tensor(image_paths)
    Y = model(X)

    embedding_dict = dict(zip(image_names, [row.tolist() for row in Y.detach().numpy()]))
    out_file = open('embeddings.json','w+')
    json.dump(embedding_dict, out_file)

    print(Y)