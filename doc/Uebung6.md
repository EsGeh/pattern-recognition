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
	|-- plots
	|   |-- ...
	|-- resource
	|   |-- ...
	|-- src
	|   |-- PatternRecogn
	|   |   `-- Gauss.hs
	|   `-- ...

Der Code ist folgendermaßen aufgeteilt:

#. Ausführbare Datei (siehe "./app")

	In der "./app/Main.hs" befindet sich der Code zum Einlesen der Test-Daten.

#. Bibliothek (siehe "./src")

	Hier befindet sich die eigentliche Funktionalität des Klassifizierungsalgorithmus

#. Eingabe- und Ausgabedateien

Im Ordner "./resource" befinden sich die Trainingsdatensätze und der Testdatensatz.
Im Ordner "./plots" befindet sich nach der Ausführung des Programms die Diagramme der Verteilungskurven entlang dem mittels der Fischerdiskrimination errechneten Vektor.

### Die Funktionalität des Programms

Das Programm wählt nacheinander alle 2-Tupel der Trainingsdatensätze in "./resource/train.\*" und klassifiziert die Testdaten in "./resource/zip.test":

~~~ {#test .haskell .numberLines startFrom="41"}
main :: IO ()
main =
	handleErrors $
	do
		mapM_
			(uncurry4 testWithData . uncurry testParamsFromLabels) $
			allPairs [3,5,7,8]
	where
		handleErrors x =
			...
~~~

Das heißt die Funktion "testWithData" wird z.B. mit den Parametern zweier Dateinamen und den entsprechenden Labels aufgerufen.

~~~ {#test .haskell .numberLines startFrom="65"}
testWithData :: FilePath -> FilePath -> Label -> Label -> ErrT IO ()
testWithData trainingFile1 trainingFile2 label1 label2 =
	do
		...
		testInput <-
			readTestInput
				trainingFile1 trainingFile2
				label1 label2
			:: ErrT IO (AlgorithmInput, Vector)
		testGauss label1 label2 testInput >>= \quality ->
			liftIO $ putStrLn $ concat $ ["gauss quality:", show $ quality]
		testProjectedGauss label1 label2 testInput >>= \quality ->
			liftIO $ putStrLn $ concat $ ["projected gauss quality:", show $ quality]
		testLinearRegression label1 label2 testInput >>= \quality ->
			liftIO $ putStrLn $ concat $ ["linear regression quality:", show $ quality]
~~~

Hier werden die geladenen Daten mittels 3-er Algorithmen klassifiziert und deren Trefferquote gemessen.
Außerdem werden im Fall der Fischerdiskrimination als Seiteneffekt errechneten Verteilungen beider Klassen als Diagramm in das Verzeichnis "plots" gespeichert.


### Ergebnis

Bei der Ausführung führt das Programm 3 verschiedene Klassifizierungsalgorithmen auf den Daten aus, und berechnet deren Trefferquote:


	----------------------------------------------
	classifying to labels [3,5] in files ["resource/train.3","resource/train.5"]
	gauss quality:0.9447852760736196
	projected gauss quality:0.9294478527607362
	linear regression quality:0.9294478527607362
	----------------------------------------------
	classifying to labels [3,7] in files ["resource/train.3","resource/train.7"]
	gauss quality:0.987220447284345
	projected gauss quality:0.9840255591054313
	linear regression quality:0.9744408945686901
	----------------------------------------------
	classifying to labels [3,8] in files ["resource/train.3","resource/train.8"]
	gauss quality:0.9367469879518072
	projected gauss quality:0.9457831325301205
	linear regression quality:0.9518072289156626
	----------------------------------------------
	classifying to labels [5,7] in files ["resource/train.5","resource/train.7"]
	gauss quality:0.9869706840390879
	projected gauss quality:0.9837133550488599
	linear regression quality:0.9837133550488599
	----------------------------------------------
	classifying to labels [5,8] in files ["resource/train.5","resource/train.8"]
	gauss quality:0.9447852760736196
	projected gauss quality:0.9631901840490797
	linear regression quality:0.9631901840490797
	----------------------------------------------
	classifying to labels [7,8] in files ["resource/train.7","resource/train.8"]
	gauss quality:0.9712460063897763
	projected gauss quality:0.9776357827476039
	linear regression quality:0.9776357827476039


Offensichtlich sind die Unterschiede von Fall zu Fall unterschiedlich. Keiner der Algorithmen ist in jedem Falle optimal.
Die Diagramme mit den Verteilungen nach der Projektion auf den Vektor $u$ der mittels Fischerdiskrimination gefunden wurde sind im Verzeichnis ".plots/" zu finden.

## Kompilieren des Programms

### Abhängigkeiten

* git (siehe <https://git-scm.com/>)
* stack (siehe <https://docs.haskellstack.org/>)

### Kompilieren

	$ git clone https://github.com/EsGeh/pattern-recognition
	$ git checkout exercise5-release
	$ stack setup
	$ stack build

### Ausführen mittels Stack

	$ stack exec patternRecogn-exe
