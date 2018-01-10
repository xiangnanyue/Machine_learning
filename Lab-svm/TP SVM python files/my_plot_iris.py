from plot_iris import make_meshgrid, plot_contours
from sklearn import svm, datasets
import numpy as np
import matplotlib.pyplot as plt
import argparse

# import some data to play with
iris = datasets.load_iris()
# Take the first two features. We could avoid this by using a two-dim dataset
X = iris.data[:, :2]
y = iris.target

def show_SVC_graph(C=1.0, gamma=0.7, degree=3):
    # define the model params 
    models = (svm.SVC(kernel='linear', C=C),
            svm.LinearSVC(C=C),
            svm.SVC(kernel='rbf', gamma=gamma, C=C),
            svm.SVC(kernel='poly', degree=degree, C=C))
    # fit the model by data
    models = (clf.fit(X, y) for clf in models)
    # title for the plots
    titles = ('SVC with linear kernel (C ' + str(C) + ')',
            'LinearSVC (linear kernel)',
            'SVC with RBF kernel (gamma '+str(gamma) +')',
            'SVC with polynomial (degree'+ str(degree) + ') kernel')

    # Set-up 2x2 grid for plotting.
    fig, sub = plt.subplots(2, 2)
    plt.subplots_adjust(wspace=0.4, hspace=0.4)

    X0, X1 = X[:, 0], X[:, 1]
    xx, yy = make_meshgrid(X0, X1)

    for clf, title, ax in zip(models, titles, sub.flatten()):
        plot_contours(ax, clf, xx, yy,
                    cmap=plt.cm.coolwarm, alpha=0.8)
        ax.scatter(X0, X1, c=y, cmap=plt.cm.coolwarm, s=20, edgecolors='k')
        ax.set_xlim(xx.min(), xx.max())
        ax.set_ylim(yy.min(), yy.max())
        ax.set_xlabel('Sepal length')
        ax.set_ylabel('Sepal width')
        ax.set_xticks(())
        ax.set_yticks(())
        ax.set_title(title)

    plt.show()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="C, gamma, degree")
    parser.add_argument("--C", default=1.0)
    parser.add_argument("--gamma", default=0.7)
    parser.add_argument("--degree", default=3)
    args = parser.parse_args()
    
    show_SVC_graph(float(args.C),float(args.gamma), float(args.degree))