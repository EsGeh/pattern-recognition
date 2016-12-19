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

* "./app/Main.hs": hier befindet sich der Code zum Einlesen der Test-Daten.

*	"./src/PatternRecogn/Perceptron.hs": Hier befindet sich die eigentliche Funktionalität des Klassifizierungsalgorithmus

#. Eingabe- und Ausgabedateien

Im Ordner "./resource" befinden sich die Trainingsdatensätze und der Testdatensatz.
Die Ausgabe erfolgt über die Standardausgabe.

### Die Funktionalität des Programms

Das Programm wählt nacheinander alle 2-Tupel der Trainingsdatensätze in "./resource/train.\*" und klassifiziert die Testdaten in "./resource/zip.test":

**"./app/Main.hs"**:

~~~ {#test .haskell .numberLines startFrom="42"}
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

**"./app/Main.hs"**:

~~~ {#test .haskell .numberLines startFrom="66"}
testWithData :: FilePath -> FilePath -> Label -> Label -> ErrT IO ()
testWithData trainingFile1 trainingFile2 label1 label2 =
	do
		...
		testInput <-
			readTestInput
				trainingFile1 trainingFile2
				label1 label2
			:: ErrT IO (AlgorithmInput, Vector)
		testPerceptron label1 label2 testInput
			>>= \quality -> liftIO $ putStrLn $ concat $ ["perceptron quality:", show $ quality]
~~~

Hier werden die geladenen Daten mittels Perzeptron-Lernen klassifiziert und deren Trefferquote gemessen.

Das Durchlauf beim Lernen funktioniert so:
Es werden *alle* Trainingsdaten durchlaufen (zunächst die mit Label1, dann die mit Label2).

**"./src/PatternRecogn/Perceptron.hs"**:

~~~ {#test .haskell .numberLines startFrom="49"}
perceptronStepAll set1 set2 param =
	perceptronStep (-1) set1
	.	
	perceptronStep 1 set2
	$
	param
~~~

Dabei wird für jedes Sample der Trainingsmenge der Gewichtsvektor $\beta$ korrigiert, falls er das Sample nicht richtig klassifiziert:

**"./src/PatternRecogn/Perceptron.hs"**:

~~~ {#test .haskell .numberLines startFrom="56"}
perceptronStep :: Label -> Matrix -> ClassificationParam -> ClassificationParam
perceptronStep expectedLabel set param =
	foldl conc param $ Lina.toRows set
	where
		conc :: ClassificationParam -> Vector -> ClassificationParam
		conc beta y = 
			let estimatedClass = classifySingleSample_extended (-1, 1) beta y
			in
				if estimatedClass == expectedLabel
				then beta
				else
					if estimatedClass < 0
					then beta + y
					else beta - y
~~~

Dieses Verfahren wird (bis maximal 1000fach) iteriert, so lange bis der Fehler klein genug ist.
Trotzdem bei jeder Iteration ("perceptronStepAll") alle Trainingsdaten durchlaufen werden ist die Laufzeit bemerkenswert schnell.

### Ergebnis

Dies ist die Ausgabe des Programms:

	$ ./scripts/run.sh
	----------------------------------------------
	classifying to labels [3,5] in files ["resource/train.3","resource/train.5"]
	perceptron quality:0.911042944785276
	----------------------------------------------
	classifying to labels [3,7] in files ["resource/train.3","resource/train.7"]
	perceptron quality:0.9776357827476039
	----------------------------------------------
	classifying to labels [3,8] in files ["resource/train.3","resource/train.8"]
	perceptron quality:0.9668674698795181
	----------------------------------------------
	classifying to labels [5,7] in files ["resource/train.5","resource/train.7"]
	perceptron quality:0.9869706840390879
	----------------------------------------------
	classifying to labels [5,8] in files ["resource/train.5","resource/train.8"]
	perceptron quality:0.9478527607361963
	----------------------------------------------
	classifying to labels [7,8] in files ["resource/train.7","resource/train.8"]
	perceptron quality:0.9840255591054313

Die Klassifizierungsqualität schwankt deutlich, ist aber in allen Fällen über 90%.

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
