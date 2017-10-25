---
author: Samuel Gfrörer
title: Mustererkennung - __TODO__ ÜbungX
date: __TODO__
documentclass: article
# institute: Freie Universität Berlin
# link-citations: true
# csl: plain.bst
# csl: din-1505-2-alphanumeric
lang: de
papersize: A4
---
# __TODO__

## Ausführen des Programms

Das Programm kann folgendermaßen ausgeführt werden:

	$ ./scripts/run_test.sh

## Implementierung

Der Code wird als Github-Repository verwaltet (siehe [](https://github.com/EsGeh/pattern-recognition)).
Um das Programm selbst zu installieren und zu kompilieren, siehe unten.

### Ordnerstruktur

	.
	|-- test
	|   `-- __TODO__.hs
	|-- resource
	|   |-- ...
	|-- src
	|   |-- PatternRecogn
	|   |   `-- __TODO__.hs
	|   `-- ...
	|-- app
	|-- plots
	|   |-- ...

Das Programm teilt sich auf in eine eine Bibliothek (siehe Verzeichnis "./src") und Tests (siehe Verzeichnis "./test").
Für diese Übung sind folgende Dateien im Quellcode relevant:

__TODO__

#. Eingabe- und Ausgabedateien

Der Test kann mit folgendem Befehl ausgeführt werden:

	$ ./scripts/run_test.sh

Im Ordner "./resource" befinden sich die Trainingsdatensätze und der Testdatensatz.
Die Ausgabe erfolgt über die Standardausgabe.

### Die Funktionalität des Tests

Der Trainingsdatensatz für die Tests befindet sich im Verzeichnis "./resource/train.\*".
Es handelt sich um einen Datensatz zur Erkennung der Ziffern 0,...,9 aus Handschrift.
Der Datensatz stammt von der Webseite zum Buch:

* Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani: An Introduction to Statistical Learning with Applications in R

Das Programm testet die Güte der Klassifizierung anhand des __TODO__-Algorithmus.

#### Ausgabe des Programms

__TODO__

Erklärung der Ausgabe:

### Ergebnis

Die entsprechenden Plots finden sich im Verzeichnis "./examplePlots/{1,2,3}". Die Standardausgabe des Programms findet sich unter "./examplePlots/{1,2,3}/log"

Die erzeugten Plots der Testläufe finden sich auch im Anhang dieses Dokuments (siehe unten).
Die folgenden Tabellen[^questionMark] fassen die Ergebnisse dreier Testdurchläufe zusammen (siehe "examplePlots/\*":

#### Diskussion der Ergebnisse

__TODO__

## Kompilieren des Programms

### Abhängigkeiten

* git (siehe <https://git-scm.com/>)
* stack (siehe <https://docs.haskellstack.org/>)

### Kompilieren

	$ git clone https://github.com/EsGeh/pattern-recognition
	$ git checkout exercise8-release
	$ stack setup
	$ stack build

# Anhang: Plots

siehe "./examplePlots"
<!-- TODO: add plots:

## AND

!["./examplePlots/1/AND.svg"](./img/1/AND.png){ width=80% }\

!["./examplePlots/2/AND.svg"](./img/2/AND.png){ width=80% }\

!["./examplePlots/3/AND.svg"](./img/3/AND.png){ width=80% }\

-->
