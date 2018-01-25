using System.Collections.Generic;
using System.Linq;

namespace CSharp
{
    public class Evaluator
    {
        public static double Correct(IEnumerable<Observation> validationSet, IClassifier classifier)
        {
            return validationSet.Select(o => Score(o, classifier))
                                .Average();
        }

        private static double Score(Observation observation, IClassifier classifer)
        {
            return classifer.Predict(observation.Pixels) == observation.Label ? 1.0 : 0.0;
        }
    }
}