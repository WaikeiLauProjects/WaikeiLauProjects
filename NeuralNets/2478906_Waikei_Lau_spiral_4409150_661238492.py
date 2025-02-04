# spiral.py
# ZZEN9444, CSE, UNSW

import torch
import torch.nn as nn
import matplotlib.pyplot as plt

class PolarNet(torch.nn.Module):
    def __init__(self, num_hid):
        super(PolarNet, self).__init__()
        # INSERT CODE HERE
        self.hidden = nn.Linear(2,num_hid)
        self.final = nn.Linear(num_hid,1)

    def forward(self, input):
        r = torch.sqrt(input[:,0]*input[:,0] + input[:,1]*input[:,1]) # CHANGE CODE HERE
        r = r.view(-1, 1)
        a = torch.atan2(input[:,1], input[:,0])
        a = a.view(-1, 1)
        dataset = torch.cat((r,a),dim=1)
        self.hlayer1 = torch.tanh(self.hidden(dataset))
        output = torch.sigmoid(self.final(self.hlayer1))
        return output

class RawNet(torch.nn.Module):
    def __init__(self, num_hid):
        super(RawNet, self).__init__()
        # INSERT CODE HERE
        self.fc1 = nn.Linear(2,num_hid)
        self.fc2 = nn.Linear(num_hid,num_hid)
        self.out = nn.Linear(num_hid,1)

    def forward(self, input):
        #output = 0*input[:,0] # CHANGE CODE HERE
        self.hlayer1 = torch.tanh(self.fc1(input))
        self.hlayer2 = torch.tanh(self.fc2(self.hlayer1))
        output = torch.sigmoid(self.out(self.hlayer2))
        return output

def graph_hidden(net, layer, node):
    #plt.clf()
    # INSERT CODE HERE
    xrange = torch.arange(start=-7,end=7.1,step=0.01,dtype=torch.float32)
    yrange = torch.arange(start=-6.6,end=6.7,step=0.01,dtype=torch.float32)
    xcoord = xrange.repeat(yrange.size()[0])
    ycoord = torch.repeat_interleave(yrange, xrange.size()[0], dim=0)
    grid = torch.cat((xcoord.unsqueeze(1),ycoord.unsqueeze(1)),1)

    with torch.no_grad(): # suppress updating of gradients
        net.eval()        # toggle batch norm, dropout
        output = net(grid)
        if layer == 1:
          pred = (net.hlayer1[:, node]>=0).float()
        
        elif layer == 2:
          pred = (net.hlayer2[:, node]>=0).float()

        net.train() # toggle batch norm, dropout back again

        #pred = (output >= 0.5).float()

        # plot function computed by model
        plt.clf()
        plt.pcolormesh(xrange,yrange,pred.cpu().view(yrange.size()[0],xrange.size()[0]), cmap='Wistia')

