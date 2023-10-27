import torch
import torchvision.models as models
import torchvision
import PIL.Image as Image
import os
import json
import torch.nn as nn

class CustomResnext50_32x4d(nn.Module):
    def __init__(self, n_out, checkpoint_path):
        super(CustomResnext50_32x4d, self).__init__()

        # Define the model architecture
        self.model = models.resnext50_32x4d()
        self.model.fc = nn.Linear(in_features=2048, out_features=n_out, bias=True)
        self.model = torch.nn.DataParallel(self.model)
        
        # Load the checkpoint within the class
        self.load_model_weights(checkpoint_path)

    def forward(self, x):
        return self.model(x)  # We are only returning the final output here

    def load_model_weights(self, checkpoint_path):
        checkpoint = torch.load(checkpoint_path, map_location='cpu')
        self.model.load_state_dict(checkpoint['model_state_dict'])

n_out = 6269
model_path = 'TC-SAY-resnext.tar'
model = CustomResnext50_32x4d(n_out=n_out, checkpoint_path=model_path)

# For testing:
sample_input = torch.randn(1, 3, 224, 224)
outputs = model(sample_input)
print(outputs.shape)  # Expected output shape, e.g., torch.Size([1, 6269])


# Assuming you have the load_images_to_tensor function from your original code
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
    device = 'cpu'  # or 'cuda' if you have a GPU

    # 2. Process Images
    image_paths = 'stims_smaller_set/padded/'
    X, image_names = load_images_to_tensor(image_paths)

    # 3. Extract Embeddings
    n_out = 6269
    model_path = 'TC-SAY-resnext.tar'
    model = CustomResnext50_32x4d(n_out=n_out, checkpoint_path=model_path)
    model.eval()  # Set the model to evaluation mode
    with torch.no_grad():  # Disable gradient calculations
        embeddings = model(X)

    # 4. Save Embeddings
    embedding_dict = dict(zip(image_names, [row.tolist() for row in embeddings.numpy()]))
    with open('resnet50_SC_embeddings.json', 'w+') as out_file:
        json.dump(embedding_dict, out_file)

    print(embeddings)
