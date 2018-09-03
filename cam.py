# -*- coding:utf-8 -*-
import torch
from torch.autograd import Function
from torchvision import models
from torchvision import utils
from torch import nn
import torch.nn.functional as F
import cv2
import sys
import numpy as np
np.seterr(divide='ignore',invalid='ignore')
from torchsummary import summary
import argparse
import shutil
import os
os.environ["CUDA_VISIBLE_DEVICES"] = "1"

# net
class vgg16(nn.Module):  #CONV_NET类继承nn.Module类
    def __init__(self):
        super(CONV_NET, self).__init__()  #使CONV_NET类包含父类nn.Module的所有属性
        # super()需要两个实参，子类名和对象self
        self.features = nn.Sequential(nn.Conv2d(6, 64, 3, 1, padding=1),
                                        nn.ReLU(inplace=True),
                                        nn.Conv2d(64, 64, 3, 1, padding=1),
                                        nn.ReLU(inplace=True))

        self.classifier = nn.Sequential(nn.Linear(2560, 128), 
                                        nn.ReLU(inplace=True),
                                        nn.Dropout(0.5),
                                        nn.Linear(128, 32),
                                        nn.ReLU(inplace=True),
                                        nn.Dropout(0.5),
                                        nn.Linear(32, 4),
                                        nn.Softmax(dim=1))

    def forward(self, x):
        x = self.features(x)
        x = x.view(x.size(0), -1)
        x = self.classifier(x)
        return x


class FeatureExtractor():
    def __init__(self, model, target_layers):
        self.model = model
        self.target_layers = target_layers
        self.gradients = []
    def save_gradient(self, grad):
        self.gradients.append(grad)
    def __call__(self, x):
        outputs = []
        self.gradients = []
        for name, module in self.model._modules.items():
            #print name
            x = module(x)
            if name in self.target_layers:
                x.register_hook(self.save_gradient)
                outputs += [x]
        #print x.shape
        return outputs, x

class ModelOutputs():
    def __init__(self, model, target_layers):
        self.model = model
        self.feature_extractor = FeatureExtractor(self.model.features, target_layers)
    def get_gradients(self):
        return self.feature_extractor.gradients
    def __call__(self, x):
        target_activations, output  = self.feature_extractor(x)
        #print output.shape
        output = output.view(-1,8192)
        output = self.model.classifier(output)
        #print output.shape
        return target_activations, output

def preprocess_image(img):
    means=[ 0.485, 0.456, 0.406]
    stds=[0.229, 0.224, 0.225]

    preprocessed_img = img.copy()[: , :, ::-1]
    for i in range(3):
        preprocessed_img[:, :, i] = preprocessed_img[:, :, i] - means[i]
        preprocessed_img[:, :, i] = preprocessed_img[:, :, i] / stds[i]
    preprocessed_img = np.ascontiguousarray(np.transpose(preprocessed_img, (2, 0, 1)))
    preprocessed_img = torch.from_numpy(preprocessed_img)
    preprocessed_img.unsqueeze_(0)
    input = preprocessed_img.requires_grad_()
    return input

def show_cam_on_image(img, mask, img_name):
    heatmap = cv2.applyColorMap(np.uint8(255*mask), cv2.COLORMAP_JET)
    heatmap = np.float32(heatmap) / 255
    cam = heatmap + np.float32(img)
    cam = cam / np.max(cam)
    cv2.imwrite(img_name, np.uint8(255 * cam))

class GradCam:
    def __init__(self, model, target_layer_names, use_cuda):
        self.model = model
        self.model.eval()
        self.cuda = use_cuda
        if self.cuda:
            self.model = model.cuda()
        self.extractor = ModelOutputs(self.model, target_layer_names)
    def forward(self, input):
        return self.model(input)
    def __call__(self, input, index = None):
        if self.cuda:
            features, output = self.extractor(input.cuda())
        else:
            features, output = self.extractor(input)
        if index == None:
            index = output.data.max(1, keepdim=True)[1]

        one_hot = np.zeros((1, output.size()[-1]), dtype = np.float32)
        one_hot[0][index] = 1
        one_hot = torch.from_numpy(one_hot)
        one_hot.requires_grad_()
        #print one_hot.cuda() * output
        if self.cuda:
            one_hot = torch.sum(one_hot.cuda() * output)
        else:
            one_hot = torch.sum(one_hot * output)
        #print one_hot #预测类别的score
        self.model.features.zero_grad()
        self.model.classifier.zero_grad()
        one_hot.backward()
        grads_val = self.extractor.get_gradients()[-1].cpu().data.numpy()

        target = features[-1]
        target = target.cpu().data.numpy()[0, :]
        weights = np.mean(grads_val, axis = (2, 3))[0, :]
        cam = np.ones(target.shape[1 : ], dtype = np.float32)
        for i, w in enumerate(weights):
            cam += w * target[i, :, :]
        cam = np.maximum(cam, 0)
        cam = cv2.resize(cam, (128, 128))
        cam = cam - np.min(cam)
        cam = cam / np.max(cam)
        return cam

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--use-cuda', action='store_true', default=False,
                        help='Use NVIDIA GPU acceleration')
    args = parser.parse_args()
    args.use_cuda = args.use_cuda and torch.cuda.is_available()
    if args.use_cuda:
        print("Using GPU for acceleration")
    else:
        print("Using CPU for computation")

    return args

if __name__ == '__main__':
    args = get_args()

    model = vgg16()
    # print model
    model.load_state_dict(torch.load('./model/135_params.pkl'))
    grad_cam = GradCam(model, target_layer_names = ["2"], use_cuda=args.use_cuda)  #default=35
    #summary(model.cuda(),(1,28,28))

    path = '/home/computer/lcy/pytorch/MyProject//fire_classification/fireimage/'

    cam_path = '/home/computer/lcy/pytorch/MyProject//fire_classification/camresult/'
    shutil.rmtree(cam_path)  # 将整个文件夹删除
    os.makedirs(cam_path)  # 创建一个文件夹
  
    target_index = 0
    mask = grad_cam(input, target_index)
    # show_cam_on_image(img, mask, cam_path+str(i)+'.jpg')

    heatmap = cv2.applyColorMap(np.uint8(255 * mask), cv2.COLORMAP_JET)
    heatmap = np.float32(heatmap) / 255
    cam = 0.8 * heatmap + np.float32(img)
    cam = cam / np.max(cam)
    cam = np.uint8(255 * cam)
    cam = cv2.resize(cam, (256, 256))
    cv2.imwrite(cam_path+fi+'_cam.jpg', cam)



