using System;

namespace CSharp
{
    class Program
    {
        static void Main()
        {
            var trainingPath = @"..\..\..\Data\trainingsample.csv";
            var training = DataReader.ReadObservations(trainingPath);

            Console.ReadLine();
        }
    }
}
