# -*- coding:utf-8 -*-
from sklearn.manifold import TSNE
from sklearn.datasets import load_iris
from sklearn.decomposition import PCA
import matplotlib
matplotlib.use('Agg')
from matplotlib import pyplot as plt
import numpy as np

all = np.loadtxt(open('./feature_csv/b1feature.csv','rb'),delimiter=',')
print all.shape
data = all[:,1:65]
target = all[:,0]
print data.shape
print target.shape

X_tsne = TSNE(learning_rate=100).fit_transform(data)
X_pca = PCA().fit_transform(data)

print X_tsne.shape

# plt.subplot(121)
plt.scatter(X_tsne[:,0],X_tsne[:,1],c=target)
# plt.subplot(122)
# plt.scatter(X_tsne[:,0],X_tsne[:,1],c=target)
plt.savefig('./tsne_pic/tsne_b1.jpg')