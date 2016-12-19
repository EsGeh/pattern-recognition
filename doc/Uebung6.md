# Fischerdiskrimination

## Ausführen des Programms

Das Programm kann folgendermaßen ausgeführt werden:

	$ ./scripts/run.sh

## Implementierung

Der gesamte Code sowie die ausführbare Datei befindet sich im beigefügten Archiv.
Der Code wird als Github-Repository verwaltet (siehe <https://github.com/EsGeh/pattern-recognition>).
Um das Programm selbst zu installieren und zu kompilieren, siehe unten.

### Ordnerstruktur

	.
	|-- app
	|   `-- Main.hs
	|-- resource
	|   |-- ...
	|-- src
	|   |-- PatternRecogn
	|   |   `-- Perceptron.hs
	|   `-- ...

Das Programm teilt sich auf in eine ausführbare Datei (siehe Verzeichnis "./app") und eine Bibliothek (siehe Verzeichnis "./src").
Für diese Übung relevanter Quellcode:

* "./app/Main.hs": hier befindet sich der Code zum Testen des Klassifizierungsalgorithmus anhand verschiedener Daten

*	"./src/PatternRecogn/NeuronalNetworks.hs": Hier befindet sich die eigentliche Funktionalität des Klassifizierungsalgorithmus

#. Eingabe- und Ausgabedateien

Im Ordner "./resource" befinden sich die Trainingsdatensätze und der Testdatensatz.
Die Ausgabe erfolgt über die Standardausgabe.

### Die Funktionalität des Programms

Das Programm trainiert nacheinander künstliche neuronale Netzwerke mittels "Backpropagation Learning" zur Erkennung folgender Funktionen:

* logisches "UND"
* logisches "ODER"
* logisches "XOR"
* Klassifizierung der Trainingsdatensätze (Erkennung von Ziffern 3,5,7,8) in "./resource/train.\*" und klassifiziert danach die Testdaten in "./resource/zip.test"

Dabei werden die Qualität der Klassifizierung über mehrere Iterationen ausgegeben.
Im Falle der logischen Verknüpfungen sind Trainingsdaten und Testdaten identisch.
Im Fall der Ziffern wird die Qualität sowohl für die Trainingsdaten als auch die Testdaten ausgegeben.

### Ergebnis

Dies ist die Ausgabe des Programms:

	$ ./scripts/run.sh

Hier wird folgendes sichtbar:

'''

## Kompilieren des Programms

### Abhängigkeiten

* git (siehe <https://git-scm.com/>)
* stack (siehe <https://docs.haskellstack.org/>)

### Kompilieren

	$ git clone https://github.com/EsGeh/pattern-recognition
	$ git checkout exercise6-release
	$ stack setup
	$ stack build

### Ausführen mittels Stack

	$ stack exec patternRecogn-exe
