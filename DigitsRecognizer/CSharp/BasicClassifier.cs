using System.Collections.Generic;

namespace CSharp
{
    public class BasicClassifier : IClassifier
    {
        private IEnumerable<Observation> _data;
        private readonly IDistance _distance;

        public BasicClassifier(IDistance distance)
        {
            _distance = distance;
        }

        public void Train(IEnumerable<Observation> trainingSet)
        {
            _data = trainingSet;
        }

        public string Predict(int[] pixels)
        {
            Observation currentBest = null;
            var shortest = double.MaxValue;

            foreach (var objs in _data)
            {
                var dist = _distance.Between(objs.Pixels, pixels);
                if (dist < shortest)
                {
                    shortest = dist;
                    currentBest = objs;
                }
            }

            return currentBest.Label;
        }
    }
}