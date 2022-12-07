import pandas as pd, json, pickle as pkl, numpy as np, pickle
from sklearn.preprocessing import MultiLabelBinarizer

with open("data/all_opportunities_df.pkl", 'rb') as infile:
    df = pkl.load(infile)


# Prefix Ages and manipulate and unify the ages group and fix an age type (9 to 6)
for i, el in enumerate(df.ages,1):
    for age in range(len(el)):
        #print('age', el[age])
        if el[age] != '':
            el[age] = "Ages_" + el[age]

print('result: ',df.ages)

# Unify REACH
df.reach = df.reach.apply(lambda x: 'Missouri' if x == ['Missouri', 'Illinois'] else x)
df.reach = df.reach.apply(lambda x: np.nan if x == [] else x)
df.reach = df.reach.apply(lambda x: str(x).strip("[]''"))
df.reach = df.reach.apply(lambda x: 'None' if x == '' else x)
df.reach = df.reach.apply(lambda x: np.nan if x == 'None' else x)

print(df.reach)
print(df.attention)
df.attention = df.attention.apply(lambda x: ['None'] if x == [] or x == None else x)
for i, row in enumerate(df.attention, 1):
    for element in range(len(row)):
        if row[element] == 'Gifted and talented students' or row[element] == 'Gifted and Talented':
            row[element] = "Att_" + "GTS"
        else:
            if row[element] =='Students with disabilities' or row[element] == 'Students with disabilites':
                row[element] = "Att_" + "SwDis"
            else:
                if row[element] == 'Students at risk from dropping out of school':
                    row[element] = "Att_" + "SRDoS"
                else:
                    row[element] = "Att_" + row[element]
print(df.attention)

# LANGUAGE unifying
df.language = df.language.apply(lambda x: ['None'] if x == [] or x == None else x)
for i, row in enumerate(df.language,1):
    for el in range(len(row)):
        row[el] = "L_" + row[el]

print(df.areaOfInterest)
# Replacing/abbreviating strings in ATTENTION
for i,row in enumerate(df.areaOfInterest,1):
    for el in range(len(row)):
        if (row[el] == 'Earth and Environmental Science' or row[el] == 'earth and environmental science' or row[el] == 'earth and Environmental Science'):
            row[el] = "AoI_EES"
        else:
            if (row[el] == 'Computer Science' or row[el] =='computer science'):
                row[el] = "AoI_CS"
            else:
                if (row[el] == 'Coding/Programming'):
                    row[el] = "AoI_" + 'Coding/Programming'
                else:
                    if (row[el] == 'General Science'):
                        row[el] = "AoI_GS"
                    else:
                        if (row[el] == 'Architecture and Design'):
                            row[el] = "AoI_AaD"
                        else:
                            if (row[el] == 'Space Science/Aerospace/Rocketry'):
                                row[el] = "AoI_SSAR"
                            else:
                                row[el] = "AoI_" + row[el]
print(df.areaOfInterest)

# TYPEOFOPPORTUNITY stripping the list brackets
df.typeOfOpportunity = df.typeOfOpportunity.apply(lambda x: str(x).strip("[]'"))

# run the OHE on AGES, ATTENTION, LANGUAGE, AREAOFINTEREST
mlb = MultiLabelBinarizer()
variables = [df.ages,df.attention, df.language, df.areaOfInterest]
for el in variables:
     df = df.join(pd.DataFrame(mlb.fit_transform(el),
                          columns=mlb.classes_,
                          index=df.index))

#Varibales Checking
for n,el in enumerate(df.reach,1):
    print(n, el)
df.reach[df.reach.isna()]
df.attention[df.attention.isna()]

for n, el in enumerate(df.attention,1):
    print(n, el)
for n, el in enumerate(df.language,1):
    print(n, el)
for n, el in enumerate(df.areaOfInterest,1):
    print(n, el)
for n, el in enumerate(df.areaOfInterest,1):
    print(n, el)

# Check are there any locationCity's format is not correct
for i in range(1, len(df)):
    if df.locationCity[i][0].isdigit():
        print(df.locationCity[i])
        location = df.locationCity[i].split(",")
        print(location)
        print(len(location[1]))
        df.locationCity[i] = location[1][1:len(location[1])]
        print(df.locationCity[i])


#Store the data
with open("cleaned_data.pkl", "wb") as outfile:
    pickle.dump(df, outfile)

df.to_csv("data/all_opportunities_df_cleaned.csv")