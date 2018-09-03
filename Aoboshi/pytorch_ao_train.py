# -*- coding:utf-8 -*-
import torch
import torch.utils.data as dataf
import torch.optim as optim
from torch import nn
import numpy as np
import json
import torch.nn.functional as F
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from torchsummary import summary
import shutil

import os
os.environ["CUDA_VISIBLE_DEVICES"] = "0"

def weights_normal_init(model, dev=0.01):
	if isinstance(model, list):
		for m in model:
			weights_normal_init(m, dev)
	else:
		for m in model.modules():
			if isinstance(m, nn.Conv2d):				
				#print torch.sum(m.weight)
				m.weight.data.normal_(0.0, dev)
				if m.bias is not None:
					m.bias.data.fill_(0.0)
			elif isinstance(m, nn.Linear):
				m.weight.data.normal_(0.0, dev)

				
class CONV_NET(nn.Module):  #CONV_NET类继承nn.Module类
	def __init__(self):
		super(CONV_NET, self).__init__()  #使CONV_NET类包含父类nn.Module的所有属性
		# super()需要两个实参，子类名和对象self
		self.features = nn.Sequential(nn.Conv2d(6, 16, 3, 1, padding=1),
										nn.ReLU(inplace=True),
										nn.Conv2d(16, 32, 3, 1, padding=1),
										nn.ReLU(inplace=True),
										nn.Conv2d(32, 32, 3, 1, padding=1),
										nn.ReLU(inplace=True),
										# nn.Conv2d(32, 64, 3, 1, padding=1),
										# nn.ReLU(inplace=True),
										nn.Conv2d(32, 64, 3, 1, padding=1),
										nn.ReLU(inplace=True))
										# nn.AdaptiveAvgPool2d(1))

		self.classifier = nn.Sequential(nn.Linear(64*5*8, 4),	
										# nn.ReLU(inplace=True),	
										# nn.Linear(32, 4),											
										nn.Softmax(dim=1))

	def forward(self, x):
		x = self.features(x)
		x = x.view(x.size(0), -1)
		y = x
		x = self.classifier(x)
		return x,y

def pre_data():
	with open("/home/computer/lcy/pytorch/MyProject/Aoboshi/train-5x5.json", 'r') as f:
		temp = json.loads(f.read())
		temp = np.array(temp)

	np.random.shuffle(temp)

	X_all = np.zeros((7558, 6, 5, 8), dtype=np.float32)
	y_all = np.zeros(7558, dtype=np.int64)

	for i in range(7558):
		AC = np.array(temp[i]['AC'])
		AC = (AC-200)/200
		GR = np.array(temp[i]['GR'])
		GR = (GR-40)/120
		RLLD = np.array(temp[i]['RLLD'])
		RLLD = (RLLD-0)/50
		RLLS = np.array(temp[i]['RLLS'])
		RLLS = (RLLS-0)/50
		RMG = np.array(temp[i]['RMG'])
		RMG = (RMG-0)/25
		RMN = np.array(temp[i]['RMN'])
		RMN = (RMN-0)/25

		X_all[i][0] = np.reshape(AC, (5, 8)).tolist()
		X_all[i][1] = np.reshape(GR, (5, 8)).tolist()
		X_all[i][2] = np.reshape(RLLD, (5, 8)).tolist()
		X_all[i][3] = np.reshape(RLLS, (5, 8)).tolist()
		X_all[i][4] = np.reshape(RMG, (5, 8)).tolist()
		X_all[i][5] = np.reshape(RMN, (5, 8)).tolist()

		LITHO = temp[i]['LITHO']
		y_all[i] = LITHO

	data_x = torch.from_numpy(X_all)
	data_y = torch.from_numpy(y_all)
	
	return data_x,data_y


def train():
	model.train()
	train_loss = 0.
	train_acc = 0.
	# 每次输入barch_idx个数据
	for batch_idx, data in enumerate(train_loader):
		# 加载数据
		img, label = data
		img, label = img.cuda(), label.cuda()
		optimizer.zero_grad()
		out = model(img)
		out = out[0]
		# 计算损失
		loss = criterion(out, label)
		train_loss += loss.item()
		# 计算准确率
		pred = torch.max(out, 1)[1]
		train_correct = (pred == label).sum()
		train_acc += train_correct.item()
		# 回传并更新梯度
		loss.backward()
		optimizer.step()

		# 输出结果
		# if batch_idx % 50 == 0: #* len(img)
		#	 print('Train Epoch: {}  [{}/{} ({:.0f}%)]  \t  Loss: {:.6f}'.format(epoch + 1, batch_idx * len(img), len(train_dataset),
		#																	   100. * batch_idx / len(train_loader),
		#																	   train_loss / ((batch_idx + 1) * len(img))
		#																	   )
		#		   )


	print('Train Epoch: {}\tTotal_Loss: {:.6f}\tTotalAcc: {:.3f}%'.format(epoch + 1, train_loss / (len(train_dataset)),
																						 100. * train_acc / (len(train_dataset))
																						 )
		  )
	torch.save(model.state_dict(), './model/'+str(epoch)+'_params.pkl')
	print('-------------------------------------------------------------------------')

	
# evaluation--------------------------------
def eval(epoch):
	model.eval()
	
	feature_total = []
	for batch_x, batch_y in val_loader:
		feature_batch = np.zeros((2561), dtype=np.float32)
		batch_x = batch_x.requires_grad_(False).cuda()
		batch_y = batch_y.requires_grad_(False).cuda()
		out = model(batch_x)		
		pred = torch.max(out[0], 1)[1]
		
		pred  = pred.cpu().detach().numpy()
		feature_batch[0] = batch_y[0].cpu().detach().numpy()  #pred[0]		
		out = out[1][0].cpu().detach().numpy()
		feature_batch[1:2561] = out
		feature_total.append(feature_batch)
	
	feature_total = np.asarray(feature_total, dtype = np.float32)
	csv_name = './feature_csv/b' + str(epoch) + "feature.csv"
	np.savetxt(csv_name,feature_total,delimiter=',')
		


if __name__=='__main__':

	# PATH_NAME = '/home/computer/lcy/pytorch/MyProject/Aoboshi/feature_csv' 
	# shutil.rmtree(PATH_NAME)
	# os.makedirs(PATH_NAME)
	
	data_x,data_y = pre_data()
	
	## 超参数
	batch_size = 5
	learning_rate = 1e-7
	num_epoches = 1000

	train_dataset = dataf.TensorDataset(data_x, data_y)
	train_loader = dataf.DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
	val_dataset = dataf.TensorDataset(data_x, data_y)
	val_loader = dataf.DataLoader(val_dataset, batch_size=1, shuffle=False)

	model = CONV_NET() 
	# weights_normal_init(model, dev=0.01)
	model.load_state_dict(torch.load('./model/999_params.pkl'))
	model = model.cuda()
	# summary(model,(6,5,8))
	# print model

	criterion = nn.CrossEntropyLoss()
	optimizer = optim.SGD(model.parameters(), lr=learning_rate, momentum=0.9, weight_decay=0.005)  # momentum=0.9,weight_decay=0.005
	
	for epoch in range(num_epoches):
		train()
		eval(epoch+1)
	print 'finish training!'	

	
	
	
	
'''
# 可视化层的参数数量
params = list(model.parameters())
k = 0
for i in params:
	 l = 1
	 print("该层的结构：" + str(list(i.size())))
	 for j in i.size():
		 l *= j
	 print("该层参数和：" + str(l))
	 print('----------------------------')
	 k = k + l
print("总参数数量和：" + str(k))

# 保存和加载整个模型
torch.save(model, '20_model.pkl')
model = torch.load('model.pkl')

# 仅保存和加载模型参数(推荐使用)
torch.save(model.state_dict(), '200_params.pkl')
model.load_state_dict(torch.load('params.pkl'))
the_model = TheModelClass(*args, **kwargs)
the_model.load_state_dict(torch.load(PATH))
'''