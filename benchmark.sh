#!/bin/bash

wget -P networks https://gist.githubusercontent.com/LeGmask/abd23a573d6a9b2f8ffd29413c84b521/raw/f9a9a7e882b12c2baf0ac7b7d1d3d3f7374f56d9/linux26.net 
wget -P networks https://gist.githubusercontent.com/LeGmask/abd23a573d6a9b2f8ffd29413c84b521/raw/deea4da2110f35a330ae9d395150be1101181acc/brainlinks.net 
wget -P networks https://gist.githubusercontent.com/LeGmask/abd23a573d6a9b2f8ffd29413c84b521/raw/deea4da2110f35a330ae9d395150be1101181acc/worm.net
wget -P networks https://gist.githubusercontent.com/LeGmask/abd23a573d6a9b2f8ffd29413c84b521/raw/deea4da2110f35a330ae9d395150be1101181acc/sujet.net
ulimit -s unlimited
echo -e "## Sujet.net benchmark" > benchmark.md
echo -e "- **Plein**" >> benchmark.md
echo -e "\`\`\`sh" >> benchmark.md
(time make run ARGS="networks/sujet.net") >> benchmark.md 2>> benchmark.md
echo -e "\`\`\`" >> benchmark.md
echo -e "- **Creux**" >> benchmark.md
echo -e "\`\`\`sh" >> benchmark.md
(time make run ARGS="-C networks/sujet.net") >> benchmark.md 2>> benchmark.md
echo -e "\`\`\`" >> benchmark.md

echo -e "## worm.net benchmark" >> benchmark.md
echo -e "- **Plein**" >> benchmark.md
echo -e "\`\`\`sh" >> benchmark.md
(time make run ARGS="networks/worm.net") >> benchmark.md 2>> benchmark.md
echo -e "\`\`\`" >> benchmark.md
echo -e "- **Creux**" >> benchmark.md
echo -e "\`\`\`sh" >> benchmark.md
(time make run ARGS="-C networks/worm.net") >> benchmark.md 2>> benchmark.md
echo -e "\`\`\`" >> benchmark.md

echo -e "## brainlinks.net benchmark" >> benchmark.md
echo -e "- **Creux**" >> benchmark.md
echo -e "\`\`\`sh" >> benchmark.md
(time make run ARGS="-C networks/brainlinks.net") >> benchmark.md 2>> benchmark.md
echo -e "\`\`\`" >> benchmark.md

echo -e "## linux26.net benchmark" >> benchmark.md
echo -e "- **Creux**" >> benchmark.md
echo -e "\`\`\`sh" >> benchmark.md
(time make run ARGS="-C networks/linux26.net") >> benchmark.md 2>> benchmark.md
echo -e "\`\`\`" >> benchmark.md