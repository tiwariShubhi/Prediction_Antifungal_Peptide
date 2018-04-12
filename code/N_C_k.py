
file_to_read = raw_input("enter txt file to read: ")
#file_to_write = input("enter csv file to write: ")
with open(file_to_read) as f:
    data = f.readlines()
#make seperate csv files for n5,c5 and n5c5
C5=[]
comp=[]
aa= list('ACDEFGHIKLMNPQRSTVWY')
k=5 #k=5 for C5 , 10 for C10 and 15 for C15
#output = open(file_to_write,'w')
for each in data:
    i=0
    if(len(each)>k ):
        str = each[0:k]+each[-(k+1):-1]
        for m1 in aa:
            count = 0
            temp = m1
            for m2 in str:
                if m2 == temp:
                    count = count + 1
            composition = (float(count) / float(k)) * 100
            print("%.2f" % composition) ,
        #print()
print(C5)

