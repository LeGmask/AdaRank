from pyvis.network import Network
import networkx as nx
from networkx.drawing.nx_pydot import write_dot
import matplotlib.pyplot as plt
import os

nt = Network('100vh', '100%')

G = nx.Graph()

# run page_rank using cli

os.system("make run ARGS=\"networks/sujet.net\"")

with open("output.pr") as noeuds:
    with open("output.prw") as scores:
        # skip first line of scores
        scores.readline()

        for noeud, score in zip(noeuds, scores):
            nt.add_node(int(noeud), label=noeud, size=float(score)*100)

with open("networks/sujet.net") as f:
    # skip first line
    f.readline()

    for idx, line in enumerate(f):
        line = line.split()
        
        nt.add_edge(int(line[0]), int(line[1]))

# print(G.edges)

# node_weight = list(nx.get_node_attributes(G,'size').values())
# print(node_weight)
# pos = nx.shell_layout(G)
# nx.draw(G, pos=pos, cmap=plt.cm.plasma, node_color=node_weight, font_color='cyan', font_size=8, width=0.5, edge_color='grey')
# plt.show()

# nt.from_nx(G)
nt.show('graph.html', notebook=False)