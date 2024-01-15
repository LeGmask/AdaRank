from pyvis.network import Network
import networkx as nx
from networkx.drawing.nx_pydot import write_dot
import matplotlib.pyplot as plt
import os

G = nx.Graph()

# run page_rank using cli

os.system("make run ARGS=\"networks/sujet.net\"")

with open("output.pr") as noeuds:
    with open("output.prw") as scores:
        # skip first line of scores
        scores.readline()

        for idx, (noeud, score) in enumerate(zip(noeuds, scores)):
            G.add_node(idx, label=str(noeud), size=float(score)*100, color=float(score)*100)

with open("networks/sujet.net") as f:
    for idx, line in enumerate(f):
        line = line.split()
        if idx == 0:
            for i in range(int(line[0])):
                G.add_node(i)
        else:
            G.add_edge(int(line[0]), int(line[1]))



node_weight = list(nx.get_node_attributes(G,'size').values())
# print(node_weight)

# nx.draw_networkx(G, cmap=plt.cm.plasma, node_color=node_weight, font_color='cyan', font_size=8, width=0.5, edge_color='grey')
# plt.show()
nt = Network('100vh', '100%')
nt.from_nx(G)
nt.show('test.html', notebook=False)