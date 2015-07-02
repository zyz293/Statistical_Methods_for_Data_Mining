setwd('C:/Users/DaTui/Desktop/winter_quarter/research/doing')
data = read.csv("combine_input_output.csv",header = T)
data_train = data[c(1:195),]
data_test = data[196,c(1:12)]
net = neuralnet(output1+output2+output3+output4+output5+output6 ~ input1+input2+input3+input4+input5+input6+input7+input8+input9+input10+input11+input12,data_train,hidden=10,rep=1000,stepmax=1e8)
compute(net,data_test)
