using System;

namespace CSharp
{
    class Program
    {
        static void Main()
        {
            const string dataPath = @"..\..\..\Data";
            var manhattan = new ManhattanDistance();

            var trainingPath = $@"{dataPath}\trainingsample.csv";
            var training = DataReader.ReadObservations(trainingPath);
            var classifier = new BasicClassifier(manhattan);
            classifier.Train(training);

            var validationPath = $@"{dataPath}\validationsample.csv";
            var validation = DataReader.ReadObservations(validationPath);

            var correct = Evaluator.Correct(validation, classifier);
            Console.WriteLine("Correctly classified: {0:P2}", correct);

            // Test functional style
            int Dist(int[] x, int[] y) => (int) manhattan.Between(x, y);
            var funcClassifier = new FunctionalExample(Dist);
            funcClassifier.Train(training);
            var functionalCorrect = Evaluator.Correct(validation, funcClassifier);
            Console.WriteLine("Correctly classified (func): {0:P2}", functionalCorrect);

            Console.ReadLine();
        }
    }
}
