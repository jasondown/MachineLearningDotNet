using System;
using System.Collections.Generic;

namespace CSharp
{
    public class FunctionalExample : IClassifier
    {
        private IEnumerable<Observation> _data;

        public FunctionalExample(Func<int[], int[], int> distance)
        {
            Distance = distance ?? throw new ArgumentNullException(nameof(distance));
        }

        public Func<int[], int[], int> Distance { get; }

        public void Train(IEnumerable<Observation> trainingSet)
        {
            _data = trainingSet;
        }

        public string Predict(int[] pixels)
        {
            Observation currentBest = null;
            var shortest = double.MaxValue;

            foreach (Observation obs in _data)
            {
                var dist = Distance(obs.Pixels, pixels);
                if (dist < shortest)
                {
                    shortest = dist;
                    currentBest = obs;
                }
            }

            return currentBest.Label;
        }
    }
}
