# Belfort

Voici un support de scripts pour l'analyse de réseaux de personnages.

1. Choisir une oeuvre.
2. Récolter les données.
3. Les transformer en un réseau.
4. L'analyser.
 
# Comment ça marche ?

1. Copier ce dossier "Belfort" sur son ordinateur en cliquant à droite sur "Clone in Desktop".
2. Installer [RStudio](http://www.rstudio.com/).
3. Ouvrir le fichier [par_ici.R](https://github.com/mtriclot/Belfort/blob/master/par_ici.R) qui devrait lancer naturellement RStudio. Si ça n'est pas le cas, alors d'abord lancer RStudio et ouvrir le fichier ensuite.
4. À la [ligne 5](https://github.com/mtriclot/Belfort/blob/master/par_ici.R#L5) du document, remplacer ce qu'il y a entre les guillemets (par défaut setwd("~/Documents/Projets R/CaraNetwork") qui correspond à l'emplacement du dossier Belfort sur ma propre machine) par l'emplacement du dossier sur votre propre machine, c'est-à-dire l'endroit où vous l'avez cloné.
5. Une fois que c'est fait, vous pouvez remplacer à la [ligne 18](https://github.com/yrochat/Belfort/blob/master/par_ici.R#L18) le premier fichier au format csv entre guillemets par votre propre fichier avec les correspondances personnage-occurrence, par exemple en faisant un glisser-déposer de l'icône du document directement dans le script (fonctionne sur Mac, quid de linux/windows ?).
6. Puis, sur la même ligne : remplacer le second fichier au format csv par le vôtre contenant les catégorisations/rôles/attributs/descriptions des personnages. Il faudrait que le nombre de catégories reste bas, par ex. "science", "technique", "politique", etc. Jeter un coup d'oeil au fichier exemple "assomoir-attr.csv".
7. Ensuite il n'y a plus qu'à sélectionner tout le document et à l'exécuter (sur Mac : "Edit" -> "Execute", ou alors le raccourci clavier "cmd+enter"), ce qui devrait créer dans le dossier "Belfort" un visuel du réseau nommé "output.pdf".
