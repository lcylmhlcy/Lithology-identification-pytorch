# -*- coding: UTF-8 -*-
# coding=utf-8 
"""
@author: Li Chengyang
@contact: 623610394@qq.com
@file: pytorch_ao_detect.py
@time: 18-4-11 下午3:52
@desc: 
"""

import torch
import torchvision
import numpy as np
from torch import nn
import torch.nn.functional as F

class CONV_NET(nn.Module):  #CONV_NET类继承nn.Module类
    def __init__(self):
        super(CONV_NET, self).__init__()  #使CONV_NET类包含父类nn.Module的所有属性
        # super()需要两个实参，子类名和对象self
        self.features = nn.Sequential(nn.Conv2d(6, 32, 3, 1, padding=1),
                                        nn.ReLU(inplace=True),
                                        nn.Conv2d(32, 64, 3, 1, padding=1),
                                        nn.ReLU(inplace=True),
                                        nn.AdaptiveAvgPool2d(1))

        self.classifier = nn.Sequential(nn.Linear(64, 4),
                                        nn.Softmax(dim=1))

    def forward(self, x):
        x = self.features(x)
        y = x
        x = x.view(x.size(0), -1)
        x = self.classifier(x)
        return x,y

model = CONV_NET()
model.double()
# model = model.load_state_dict(torch.load('20_params.pkl'))
model.load_state_dict(torch.load('./model/100_params.pkl'))
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

detect = np.zeros((row,6, 5, 8), dtype = np.float)
result = np.zeros((row,5), dtype = np.float)
feature = np.zeros((row,65), dtype = np.float)

for i in range(0,row):
    temp_AC = data_AC[i:i+5].tolist()
    AC = [x[1:9] for x in temp_AC]
    AC = np.array(AC)
    AC = (AC-200)/200
    AC.tolist()

    temp_GR = data_GR[i:i+5].tolist()
    GR = [x[1:9] for x in temp_GR]
    GR = np.array(GR)
    GR = (GR-40)/120
    GR.tolist()

    temp_RLLD = data_RLLD[i:i+5].tolist()
    RLLD = [x[1:9] for x in temp_RLLD]
    RLLD = np.array(RLLD)
    RLLD = (RLLD-0)/50
    RLLD.tolist()

    temp_RLLS = data_RLLS[i:i+5].tolist()
    RLLS = [x[1:9] for x in temp_RLLS]
    RLLS = np.array(RLLS)
    RLLS = (RLLS-0)/50
    RLLS.tolist()

    temp_RMG = data_RMG[i:i+5].tolist()
    RMG = [x[1:9] for x in temp_RMG]
    RMG = np.array(RMG)
    RMG = (RMG-0)/25
    RMG.tolist()

    temp_RMN = data_RMN[i:i+5].tolist()
    RMN = [x[1:9] for x in temp_RMN]
    RMN = np.array(RMN)
    RMN = (RMN-0)/25
    RMN.tolist()

    # detect[i][0][0][0] = temp_depth
    detect[i][0] = AC
    detect[i][1] = GR
    detect[i][2] = RLLD
    detect[i][3] = RLLS
    detect[i][4] = RMG
    detect[i][5] = RMN

# detect = np.transpose(detect,(0,2,3,1))
detect_last = []
for i in range(0,row):
    result[i][0] = float(data_AC[i + 3][0])

for i in range(0, row):
    detect_last = np.expand_dims(detect[i], axis=0)
    detect_last = torch.from_numpy(detect_last)
    temp_pro = model(detect_last)
    print temp_pro[0].shape
    print temp_pro[1].shape
    # temp_pro = temp_pro.item().view(-1, 4)
    # temp_pro = temp_pro.numpy()
    pred = torch.max(temp_pro[0], 1)[1]
    print pred
    feature[i][0] = pred.detach().numpy()
    feature[i][1:65] = temp_pro[1][0][:][0][0].detach().numpy()
    
    # print temp_pro
    # result[i][1] = float(temp_pro[0][1])
    # result[i][2] = float(temp_pro[0][2])
    # result[i][3] = float(temp_pro[0][3])
    # result[i][4] = float(temp_pro[0][0])

np.savetxt("太23feature.csv",feature,delimiter=',')

