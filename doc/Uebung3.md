# Klassifikation mit Normalvergteilungen

## Ausführen des Programms

Das Programm kann folgendermaßen ausgeführt werden:

	$ ./scripts/run.sh

## Implementierung

Der gesamte Code sowie die ausführbare Datei befindet sich im beigefügten Archiv.
Der Code wird als Github-Repository verwaltet (https://github.com/EsGeh/pattern-recognition).
Um das Programm selbst zu installieren und zu kompilieren, siehe unten.

### Ordnerstruktur

	.
	|-- app
	|   `-- Main.hs
	|-- resource
	|   |-- ...
	|-- src
	|   |-- PatternRecogn
	|   |   `-- Gauss.hs
	|   `-- ...

Der Code ist folgendermaßen aufgeteilt:

#. Ausführbare Date (siehe "./app")

	In der "./app/Main.hs" befindet sich der Code zum Einlesen der Test-Daten.

#. Bibliothek (siehe "./src")

	Hier befindet sich die eigentliche Funktionalität des Klassifizierungsalgorithmus

Im Ordner "./resource" befinden sich die Trainingsdatensätze und der Testdatensatz.

### Die Funktionalität des Programms

Das Programm wählt nacheinander alle 2-Tupel der Trainingsdatensätze in "./resource/train.\*" und klassifiziert die Testdaten in "./resource/zip.test":

~~~ {#test .haskell .numberLines startFrom="33"}
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

Das heißt die Funktion "testWithData" wird z.B. mit den Parametern zweier Dateinamen und den entsprechenden Labels aufgerufen:

~~~ {#test .haskell .numberLines startFrom="49"}
testWithData :: FilePath -> FilePath -> Label -> Label -> ErrT IO ()
testWithData trainingFile1 trainingFile2 label1 label2 =
	do
		...
		[trainingSet1, trainingSet2] <-
			mapM (readData trainingDataFormat) $
			[ trainingFile1
			, trainingFile2
			]
		(inputLabels, inputData) <-
			prepareInputData (`elem` [fromIntegral label1, fromIntegral label2]) <$>
			readData inputDataFormat "resource/zip.test"
		let classificationParam = calcClassificationParams trainingSet1 trainingSet2
		let classified = classify (label1,label2) classificationParam inputData
		let result =  calcClassificationQuality (cmap round $ inputLabels) classified
		liftIO $ putStrLn $
			descriptionString
				trainingSet1
				trainingSet2
				classificationParam
				inputData
				classified
				result
~~~

Die beiden Trainingsdatensätze werden aus den Dateinamen in die Variablen "trainingSet1" und "trainingSet2" geladen.
Danach werden die Testdaten in die Variable "inputData" geladen.
Die Labels sind für den Testdatensatz bekannt, und werden in der Variablen "inputLabels" gehalten.
Die Funktion "calcClassificationParams" berechnet die Parameter für die Klassifizierung.
Die Funktion "classify" klassifiziert dann die Testdaten anhand der errechneten Information.
Die letzten beiden Befehle berechnen die Qualität der Klassifizierung, und geben sie aus.

Der Code für die Vorberechnung und das Klassifizieren befindet sich in "./src/PatternRecogn/Gauss.hs":
Die Vorberechnung werden im folgenden Code-Ausschnitt deutlich:

~~~ {#test .haskell .numberLines startFrom="17"}
data ClassificationParam
	= ClassificationParam {
		min1 :: Vector,
		min2 :: Vector,
		covariance1 :: Matrix,
		covariance2 :: Matrix
	}

calcClassificationParams :: Matrix -> Matrix -> ClassificationParam
calcClassificationParams set1 set2 =
	ret
	where
		ret = ClassificationParam {
			min1 = average $ toRows set1,
			min2 = average $ toRows set2,
			covariance1 = cov_SAFE (min1 ret) set1,
			covariance2 = cov_SAFE (min2 ret) set2
		}
~~~
Der Typ "ClassificationParam" speichert das Ergebnis der Vorberechnung.
Die Zeilen der übergebenen Matrizen "set1" und "set2" enthalten die Features jeweils eines Samples.
"min1" und "min2" stehen für den Erwartungswert dieser Feature-Vektoren, hier also deren Durchschnitt.
Außerdem werden die Kovarianz-Matrizen beider Trainingsdatensätze berechnet.

Die Funktion "classify" klassifiziert die Testdaten anhand der oben beschriebenen "ClassificationParam":

~~~ {#test .haskell .numberLines startFrom="72"}
	classify :: (Label, Label) -> ClassificationParam -> Matrix -> Lina.Vector Label
	classify (labelNeg, labelPos) ClassificationParam{..} =
		fromList
		.
		map classifySingleVec
		.
		toRows
		where
			classifySingleVec :: Vector -> Label
			classifySingleVec x =
				if
					mahalanobis min1 covariance1 x > mahalanobis min2 covariance2 x
				then
					labelNeg
				else
					labelPos
~~~

Die Testdaten werden als Matrix übergeben, deren Zeilen die Features der zu klassifizierenden Samples enthalten.
Für jedes Sample wird jeweils berechnet, welche Klasse gemessen mit der Mahalanobis-Distanz näher liegt und dementsprechend sortiert.

## Kompilieren des Programms

### Abhängigkeiten

* [git]
* [stack]

[git]: https://git-scm.com/
[stack]: https://docs.haskellstack.org/

### Kompilieren

	$ git clone https://github.com/EsGeh/pattern-recognition
	$ git checkout gaussClassification
	$ stack build

### Ausführen mittels Stack

	$ stack exec patternRecogn-exe
