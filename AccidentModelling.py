
from collections import Counter
import numpy as np
from numpy import mean
from numpy import std
from sklearn.model_selection import cross_val_score
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder
from sklearn.preprocessing import PowerTransformer
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import RepeatedStratifiedKFold
import pandas as pd
from collections import Counter
from sklearn.metrics import fbeta_score
from sklearn.metrics import make_scorer
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import StratifiedKFold
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.svm import SVC
from imblearn.pipeline import make_pipeline
from imblearn.under_sampling import RandomUnderSampler
from imblearn.over_sampling import SMOTENC
from imblearn.combine import SMOTEENN
from imblearn.combine import SMOTETomek
from imblearn.under_sampling import TomekLinks
from sklearn.neighbors import KNeighborsClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.svm import LinearSVC

from sklearn.naive_bayes import GaussianNB
from matplotlib import pyplot as plt

from imblearn.pipeline import Pipeline
from imblearn.over_sampling import SMOTE
from imblearn.under_sampling import EditedNearestNeighbours
from imblearn.under_sampling import NearMiss

from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import BaggingClassifier
from sklearn.ensemble import AdaBoostClassifier
from sklearn.svm import SVC
from sklearn.dummy import DummyClassifier
from sklearn.ensemble import VotingClassifier

def load_dataset(full_path):
    df = pd.read_csv(full_path)
    cat_vars = ["Accident_Severity", "Day_of_Week", "X1st_Road_Class", "Road_Type", "Junction_Detail",
                "Pedestrian_Crossing.Physical_Facilities", "Urban_or_Rural_Area", "Journey_Purpose_of_Driver",
                "Sex_of_Driver", "Age_Band_of_Driver", "Vehicle_Manoeuvre"]
    for col in cat_vars:
        df[col] = df[col].astype('category')
    y = df.pop('Accident_Severity')
    X = df
    return X,y

#Returns F2 score
def f2_measure(y_true, y_pred):
    return fbeta_score(y_true,y_pred, beta = 2,average = 'weighted')


#Perform cross validation training on the models
def evaluate_model(X,y,model):
    cv = RepeatedStratifiedKFold(n_splits = 10, n_repeats=2, random_state=1 )
    metric = make_scorer(f2_measure)
    scores = cross_val_score(model, X,y, scoring= metric, cv = cv, n_jobs=-1)
    return scores


#Models to be used for spot checking
def get_models():
    models,names = list(),list()
    models.append(LogisticRegression(solver = 'liblinear'))
    names.append('LR')
    models.append(LinearDiscriminantAnalysis())
    names.append('LDA')
    models.append(GaussianNB())
    names.append('NB')
    models.append(DecisionTreeClassifier(criterion="entropy", max_depth=3))
    names.append('DT')
    models.append(LinearSVC(random_state=123,dual=False))
    names.append('Linear SVC')
    ##Bagging
    models.append(BaggingClassifier(n_estimators=1000))
    names.append('BAG')
    # RF
    models.append(RandomForestClassifier(n_estimators=100))
    names.append('RF')
    return models,names

#Selected 3 algorithms are tested for the sampling algorithms
def get_models_for_sampling():
    models,names = list(),list()
    models.append(LogisticRegression(solver = 'lbfgs',max_iter=5000))
    names.append('LR')
    models.append(RandomForestClassifier(n_estimators=100))
    names.append('RF')
    models.append(AdaBoostClassifier(DecisionTreeClassifier(max_depth=1),n_estimators=100))
    names.append('Adaboost')
    return models,names


def get_cost_Sensitive_models():

    models, names = list(), list()
# LR
    models.append(LogisticRegression(solver='lbfgs', class_weight={1:2.0,2:0.8,3:0.4},max_iter=5000))
    names.append('LR')
# SVM
#    models.append(SVC(gamma='scale', class_weight={1:2.0,2:0.8,3:0.4}))
#    names.append('SVM')
# RF
    models.append(RandomForestClassifier(n_estimators=100,class_weight={1:2.0, 2:0.6,3:0.1}))
    names.append('RF')
    return models, names

#Load the dataset
X_train,y_train = load_dataset('C:/Users/harsh/Documents/R/Project1/train_set_wo_norm.csv')
X_val,y_val = load_dataset('C:/Users/harsh/Documents/R/Project1/validation_set_wo_norm.csv')

print(X_train.describe())
cat_ix = X_train.select_dtypes(include=['object','boolean']).columns
num_ix = X_train.select_dtypes(include = ['int64', 'float64']).columns

print(X_train.shape)
#distribution of samples across classes
print(Counter(y_train))

models,names = get_models()
train_results=list()

#Spot checking the algorithms
for i in range(len(models)):
    # one hot encode categorical, normalize numerical
    ct = ColumnTransformer([('c', OneHotEncoder(), cat_ix), ('n', MinMaxScaler(), num_ix)])
    # wrap the model in a pipeline
    pipeline = Pipeline(steps=[ ('t',ct),('m', models[i])])
    # evaluate the model and store results
    scores = evaluate_model(X_train,y_train,pipeline)
    train_results.append(scores)

#Plot the results on a box and whisker plot
plt.boxplot(train_results,labels=newnames, showmeans=True)
plt.show()

#Perform Sampling
sampler1 = TomekLinks(sampling_strategy ='majority')
X_enn,y_enn=sampler1.fit_resample(X_train,y_train)
print('TomekLinks counters')
print(Counter(y_enn))

sampler2 = NearMiss(version=1,n_neighbors=3)
X_nearmiss,y_nearmiss= sampler2.fit_resample(X_train,y_train)
print('Near miss counters')
print(Counter(y_nearmiss))

#spot check algorithms
models,names = get_models_for_sampling()
newnames=list()
train_results = list()
test_results = list()

for i in range(len(models)):
    # evaluate the model and store results
    scores = evaluate_model(X_enn,y_enn,models[i])
    train_results.append(scores)
    # summarize and store
    print('training error TomesLinks %s %.3f (%.3f)' % (names[i], np.mean(scores), np.std(scores)))
    models[i].fit(X_enn, y_enn)
    yhat = models[i].predict(X_val)
    test_score = f2_measure(y_val, yhat)
    test_results.append(test_score)
    newnames.append('TL '+names[i])
    print('test score TomesLinks %s %.3f' % (names[i], test_score))

    scores = evaluate_model(X_nearmiss, y_nearmiss, models[i])
    train_results.append(scores)
    print('training error NearMiss %s %.3f (%.3f)' % (names[i], np.mean(scores), np.std(scores)))
    models[i].fit(X_nearmiss,y_nearmiss)
    yhat = models[i].predict(X_val)
    test_score = f2_measure(y_val,yhat)
    test_results.append(test_score)
    newnames.append('NM '+names[i])
    print('test score NearMiss%s %.3f' %(names[i], test_score))

plt.boxplot(train_results,labels=newnames, showmeans=True)
plt.xticks(rotation = 90)
plt.show()
plt.savefig('C:/Users/harsh/Documents/Python/Python-Practice/my_plot.png')
np.save('results.npy',train_results)