import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("Intel_CPUs.csv")

df.to_excel("Intel_CPUs.xlsx",index = False)
