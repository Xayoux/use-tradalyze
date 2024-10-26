# use-tradalyze

Ce répertoire contient un code d'exemple d'utilisation du package `tradalyze` afin de mener une analyse de compétitivité sur la bière et le vin en se concentrant majoritairement sur l'Italie et la France.

## Utilisation

Pour utiliser ce code, le plus simple (sans cloner ce répertoire) est de télécharger le dossier zip à partir de `Code` $\rightarrow$ `Download ZIP` puis de décompresser le répertoire à l'endroit souhaité.

L'utilisateur n'a ensuite plus qu'à exécuter le fichier `use-tradalyze.Rproj` afin de lancer le projet R associé. Le script d'exemple se trouve dans le dossier `code` et se nomme `code-tradalyze.R` et est normalement auto suffisant. Il suffit d'exécuter chaque ligne les unes après les autres !

## Possibles problèmes

Le package `tradalyze` et ce script d'exemple ont été conçus avec une version de R \>= 4.4.0. Il est conseillé d'avoir au minimum cette version de R afin de s'assurer du bon fonctionnement du code.

Le package `renv` n'a pas été utilisé dans cet exemple afin de montrer comment télécharger la bonne version du package `tradalyze` à partir de github (la version de la branche principale n'est pas correcte). Dans le cas d'une vraie analyse, il est conseillé d'utiliser `renv` pour assurer au maximum la reproductibilité.

Le code va tester et télécharger les packages si besoin il y a de les télécharger.

Il est possible que l'absence de Rtools pose quelques problèmes. Si jamais c'est le cas, il suffit de télécharger la version [4.4](https://cran.r-project.org/bin/windows/Rtools/) de Rtools et de l'installer.

Si jamais le package `rlang` n'est pas déjà téléchargé et qu'une erreur se produit lors de la vérification de l'installation des packages, il suffit de le télécharger avec la commande `install.packages("rlang")`.
