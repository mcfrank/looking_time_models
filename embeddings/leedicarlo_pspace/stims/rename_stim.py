import os
import numpy as np
import re 

files = os.listdir()
count = 0
for f in np.sort(files):
    if 'inanimate' in f and 'left' in f:
        stim_num = int(re.findall(r'\d+', f)[0])
        
        count += 1
        if count < 10:
            new_filename = 'inanimate_00' + str(count) + '_left.png'
        elif count < 100:
            new_filename = 'inanimate_0' + str(count) + '_left.png'
        else:
            new_filename = 'inanimate_' + str(count) + '_left.png'
        
        os.rename( f,  new_filename)  

        # do the same for right facing ones
        os.rename(f.replace('left', 'right'), new_filename.replace('left', 'right'))  
    