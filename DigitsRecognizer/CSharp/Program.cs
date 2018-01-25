using System;

namespace CSharp
{
    class Program
    {
        static void Main()
        {
            const string dataPath = @"..\..\..\Data";

            var trainingPath = $@"{dataPath}\trainingsample.csv";
            var training = DataReader.ReadObservations(trainingPath);
            var classifier = new BasicClassifier(new ManhattanDistance());
            classifier.Train(training);

            var validationPath = $@"{dataPath}\validationsample.csv";
            var validation = DataReader.ReadObservations(validationPath);

            var correct = Evaluator.Correct(validation, classifier);
            Console.WriteLine("Correctly classified: {0:P2}", correct);

            Console.ReadLine();
        }
    }
}
