# -*- coding: UTF-8 -*-
# coding=utf-8 
"""
@author: Li Chengyang
@contact: 623610394@qq.com
@file: pytorch_ao_detect.py
@time: 18-4-11 下午3:52
@desc: 
"""

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import torch
import torchvision
import numpy as np
from torch import nn
import torch.nn.functional as F

class CONV_NET(nn.Module):  #CONV_NET类继承nn.Module类
    def __init__(self):
        super(CONV_NET, self).__init__()  #使CONV_NET类包含父类nn.Module的所有属性
        # super()需要两个实参，子类名和对象self
        self.conv1 = nn.Conv2d(6, 64, 3, 1, padding=1)
        self.conv2 = nn.Conv2d(64, 64, 3, 1, padding=1)
        self.fc1 = nn.Linear(2560, 128)
        self.relu1 = nn.ReLU(inplace=True)
        self.drop1 = nn.Dropout(0.5)
        self.fc2 = nn.Linear(128, 32)
        self.relu2 = nn.ReLU(inplace=True)
        self.drop2 = nn.Dropout(0.5)
        self.fc3 = nn.Linear(32, 4)
        self.softmax = nn.Softmax(dim=1)


    def forward(self, x):
        x = self.conv1(x)
        y = self.conv2(x)
        return x,y

model = CONV_NET()

model.load_state_dict(torch.load('200_params.pkl'))

model.eval()

datafile = '太23'
row = 723     #(707  723)

file_name = '/home/computer/lcy/pytorch/MyProject/Aoboshi/SmoothSpectrum/data/test/' + datafile
data_AC = np.loadtxt(open(file_name +'/AC.csv','rb'), delimiter=",",skiprows=1)
data_GR = np.loadtxt(open(file_name +'/GR.csv','rb'), delimiter=",",skiprows=1)
data_RLLD = np.loadtxt(open(file_name +'/RLLD.csv','rb'), delimiter=",",skiprows=1)
data_RLLS = np.loadtxt(open(file_name +'/RLLS.csv','rb'), delimiter=",",skiprows=1)
data_RMG = np.loadtxt(open(file_name +'/RMG.csv','rb'), delimiter=",",skiprows=1)
data_RMN = np.loadtxt(open(file_name +'/RMN.csv','rb'), delimiter=",",skiprows=1)

detect = np.zeros((1,6, 5, 8), dtype = np.float32)


temp_AC = data_AC[0:5].tolist()
AC = [x[1:9] for x in temp_AC]
AC = np.array(AC)
AC = (AC-200)/200
AC.tolist()

temp_GR = data_GR[0:5].tolist()
GR = [x[1:9] for x in temp_GR]
GR = np.array(GR)
GR = (GR-40)/120
GR.tolist()

temp_RLLD = data_RLLD[0:5].tolist()
RLLD = [x[1:9] for x in temp_RLLD]
RLLD = np.array(RLLD)
RLLD = (RLLD-0)/50
RLLD.tolist()

temp_RLLS = data_RLLS[0:5].tolist()
RLLS = [x[1:9] for x in temp_RLLS]
RLLS = np.array(RLLS)
RLLS = (RLLS-0)/50
RLLS.tolist()

temp_RMG = data_RMG[0:5].tolist()
RMG = [x[1:9] for x in temp_RMG]
RMG = np.array(RMG)
RMG = (RMG-0)/25
RMG.tolist()

temp_RMN = data_RMN[0:5].tolist()
RMN = [x[1:9] for x in temp_RMN]
RMN = np.array(RMN)
RMN = (RMN-0)/25
RMN.tolist()

detect[0][0] = AC
detect[0][1] = GR
detect[0][2] = RLLD
detect[0][3] = RLLS
detect[0][4] = RMG
detect[0][5] = RMN

detect_last = []

detect_last = np.expand_dims(detect[0], axis=0)
detect_last = torch.from_numpy(detect_last)
temp_pro = model(detect_last)
temp_pro1 = temp_pro[0].detach().numpy()
temp_pro2 = temp_pro[1].detach().numpy()
print temp_pro1.shape
print temp_pro2.shape
print '------------------------------------------------'
print temp_pro1[0][1][:][:]
print '------------------------------------------------'
print temp_pro2[0][1][:][:]

# plt.imshow(temp_pro[0][1][:][:])
# plt.axis('off')
# plt.savefig('1.jpg')
