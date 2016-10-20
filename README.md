# MachineLearning

#This file will contain my Machine Learning personal on going or finished project

#Classification on Hand Written Digital Number 
#This project is corporate with my peers- Winnie Yiu

#The goal here is quite easy to understand. We have (processed) images of digits, i.e., 0, 1, 2, 3, ..., 9. These come from, e.g., mail envelopes and correspond to digits in the ZIP code to which the mail is being sent. The delivery system digitally scans the ZIP code and wants to be able to recognize the ZIP code to be able to automate the sorting and initial delivery of the mail. These are typically hand-written and vary greatly, making it hard to recognize some digits. Rather than recognizing the sequence of 5 digits, we'll recognize each one separately.

#The data we have are for 5,000 images, each displaying one digit. Each image is a 28 x 28 grid of pixels, for 784 pixels. The value of each pixel ranges from 0 to 255, with 0 being "white" or empty, and 255 being black. While the pixels correspond to rows and columns in the image, they are provided as columns in the data frame. Pixel1 corresponds to the top-left corner of the image, pixel2 the next pixel down from this and in the first column, pixel3 the next one down, pixel28 the final one in the first column, and pixel29 the first in the second column.
