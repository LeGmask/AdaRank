from pyvis.network import Network
import os

nt = Network('100vh', '100%')

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

nt.show('graph.html', notebook=False)