#Simulate grid sampling and prediction error by using a Gaussian Process with RBF kernel
#Franziska Brändle 2024 based on work by Charley Wu 2017

import numpy as np
import GPy
import json
import random
from matplotlib import pyplot as plt
from sklearn import preprocessing
plt.ioff()

#function to sample values from a gaussian process kernel and create grids
def samplePrior(k, xmin=0, xmax=29, normalize = True):
	"""Samples a function from a gaussian process kernel (k), where xmin and xmax specify the square bounds of the 2D function, and normalize=True scales the payoffs between 0 and 1 """
	#Create grid using xmin and xmax
	xx, yy = np.mgrid[xmin:xmax+1, xmin:xmax+1]
	X = np.vstack((xx.flatten(), yy.flatten())).T
	K = k.K(X) #compute covariance matrix of x X
	s = np.random.multivariate_normal(np.zeros(X.shape[0]), K) #GP prior distribution
	s = np.reshape(s, (-1, 1)) #convert to 2D array
	#set min-max range for scaler
	min_max_scaler = preprocessing.MinMaxScaler(feature_range=(0,1))
	#scale to range
	if normalize==True:
		s = min_max_scaler.fit_transform(s)

	t = s.tolist()

	#convert into JSON object
	jsonData = {}
	counter = 0
	for x1 in range(xmin,xmax+1):
		for x2 in range(xmin,xmax+1):
			jsonData[counter] = {'x1':x1, 'x2':x2, 'y':t[counter]}
			counter+=1
	return jsonData


#function to calculate mean square error between models that sample randomly on the grid and the groundTruth
def simulateRandom(dataGP):

	msePerGrid = []
	mseDifferencePerGrid = []

	#go through all the generated grids
	for n in range(len(dataGP)):
		print("Grid number " + str(n))

		currentGrid = dataGP[n]
		currentData = []
		groundTruthY = []
		groundTruthX = []
		msePerStep = []
		mseDifferencePerStep = []
		mseOld = -1

		#get x and y values for the current grid
		for i in range(len(currentGrid)):
			groundTruthY.append(currentGrid[i].get('y'))
			currentXvalues = []
			currentXvalues.append(currentGrid[i].get('x1'))
			currentXvalues.append(currentGrid[i].get('x2'))
			groundTruthX.append(currentXvalues)

		#sample n values (iterations) per grid and calculate new models
		for iterations in range(numberIterations):
			#get random value that hasn't been sampled
			coordinate = random.randint(0, 899)
			while currentGrid[coordinate] == None:
				coordinate = random.randint(0, 899)
			currentData.append(currentGrid[coordinate])
			currentGrid[coordinate] = None
			#get x and y values of the random sample and the samples before
			Xvalues = []
			Yvalues = []
			for i in range(len(currentData)):
				currentXvalues = []
				currentXvalues.append(currentData[i].get('x1'))
				currentXvalues.append(currentData[i].get('x2'))
				Xvalues.append(currentXvalues)
				Yvalues.append(currentData[i].get('y'))
			XvaluesArray = np.array(Xvalues)
			YvaluesArray = np.array(Yvalues)
			#create and optimize model based on the samples
			fittingKernel = GPy.kern.RBF(input_dim=2)
			newModel = GPy.models.GPRegression(XvaluesArray, YvaluesArray,fittingKernel)
			newModel.optimize(messages=False,max_f_eval = 1000)
			#predict data for the whole grid from the model
			predictedData = newModel.predict_noiseless(np.array(groundTruthX))
			predictedDataMean = predictedData[0]
			#calculate mean squared error between the predictions and the groundtruth
			mse =((predictedDataMean - groundTruthY) ** 2).sum()
			#calculate difference between new model and old model
			if(mseOld != -1):
				mseDifference = mseOld-mse
				mseDifferencePerStep.append(mseDifference)
			msePerStep.append(mse)
			mseOld = mse
		#append the data of the current grid to the overall arrays
		msePerGrid.append(msePerStep)
		mseDifferencePerGrid.append(mseDifferencePerStep)

	return msePerGrid, mseDifferencePerGrid


#run
samples = 1000 #number of grids per lambda
outputData = {}
gridCounter=0
numberIterations = 150 #number of samples per grid

#Create and save output data
for n in range(-2,5):
	kernel10 = GPy.kern.RBF(input_dim=2, variance=1, lengthscale=(2**n))

	for sample in range(samples):
		(outputData[sample+(gridCounter*samples)]) = samplePrior(kernel10)
		print(sample)
	gridCounter +=1


#Run simulation on created grids
mseSimulation, mseDifferenceSimulation = simulateRandom(outputData)
#divide mse data into the different lambdas and get the mean
for i in range (7):
	actualCurrentLambda = 2**(i-2)
	currentLambdaString = str(actualCurrentLambda)
	gridsOfCurrentLambdaMse = []
	meanOfCurrentLambdaMse = []
	gridsOfCurrentLambdaDifference = []
	meanOfCurrentLambdaDifference = []
	for j in range(samples):
		gridsOfCurrentLambdaMse.append(mseSimulation[i*samples+j])
		gridsOfCurrentLambdaDifference.append(mseDifferenceSimulation[i*samples+j])
	for k in range (numberIterations):
		currentIndexMse = []
		for l in range (samples):
			currentIndexMse.append(gridsOfCurrentLambdaMse[l][k])
		meanOfCurrentLambdaMse.append(np.array(currentIndexMse).mean())
	for m in range (numberIterations-1):
		currentIndexDifference = []
		for n in range (samples):
			currentIndexDifference.append(gridsOfCurrentLambdaDifference[n][m])
		meanOfCurrentLambdaDifference.append(np.array(currentIndexDifference).mean())
	savestring1 = "Sampling_Simulation\Lambda=" + currentLambdaString + "Difference.txt"
	savestring2 = "Sampling_Simulation\Lambda=" + currentLambdaString + "Mse.txt"
	np.savetxt(savestring1, gridsOfCurrentLambdaDifference)
	np.savetxt(savestring2, gridsOfCurrentLambdaMse)
