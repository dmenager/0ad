import pandas as ps 
import numpy as np
from scikit import KMeans
from scikit import svm

# opens file and puts data into numpy array
X = ps.read_csv("data.csv")

# initializes kmeans clustering
kmeans = KMeans()

# builds clustering model
kmeans.fit(X)

# labels our training data
Y = kmeans.predict(X)

# number of stages we are shifting
n = 5

# shifts the data in the labels and removes the excess data in X
np.delete(Y, range(0, n), 0)
np.delete(X, range(len(X) - n, len(X)), 0)
	
# initializes SVM
clf = svm.SVC()

# builds classification model
clf.fit(X, Y)