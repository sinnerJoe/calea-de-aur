# calea-de-aur

### Use stack to build the program:
0. Install wxwidgets with version between 2.9 and 3.0.3
1. ```stack init```
2. ```stack setup```
3. ```stack build``` or ```stack install```


## The program takes as input 2 files:
* 'input.i'  - contains n and m integers separated by space on the first line followed by n lines with m quantities of gold
* 'calitate' - contains n line of m integers that specify the quality of gold ores in the cells.

Each cell in the field has a quantity and a quallity attribute. 
### *The files must be in the folder from which you execute the program!*
### *You can only select qualities in the range 1-4!*
## Functionality:
- Read/write info about the field
- Add/remove a line/column from/to field
- Mark the lines which contain cells with quality 1 or 2
- Find and mark the biggest rectangle which has the vertexes of a selected quality
- Find and mark the path from top-left corner to botttm-right corner which contains the biggest value of collected ores. You can move only down or right.

Screenshots:


 ![Calea de aur](https://s14.postimg.cc/3zdk55u81/calea_de_aur.png)


![Dreptunghi Maxim](https://s14.postimg.cc/88ia7c575/dreptunghi_maxim.png)


![Lines 1-2](https://s14.postimg.cc/pyjysdqht/lines1-2.png)
