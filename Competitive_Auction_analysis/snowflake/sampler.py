import random
import pandas as pd
import numpy as np

order = pd.read_csv("Order.csv")
pSize = np.array([0,1,2,3,4])
storeTable = np.array([0,1,2,3,4])
cheeseType = np.array([0,1,2])
toppingType = np.array([0,1,2,3])
dough = np.array([0,1,2])

# change `n` below to change the number of 
n=50
for i in range(n):
    values = []
    # store location key
    values.append(random.choice(storeTable))
    # Month
    values.append(random.randint(1,12))
    # Year
    values.append(random.randint(2015,2019))
    # Pizza Size Key
    values.append(random.choice(pSize))
    # Dough Key
    values.append(random.choice(dough))
    # Cheese Type Key
    values.append(random.choice(cheeseType))
    # Topping type key
    values.append(random.choice(toppingType))
    # Qunatity
    qty = random.choice(range(1,10))*20
    values.append(qty)
    # Profit key
    values.append(random.choice(range(qty,qty*4))*1.5)
    #put all those values in dataframe row
    order.loc[i,:] = values

print(order.head())
print(order.columns)

order.to_csv("order_random_sampling.csv")
