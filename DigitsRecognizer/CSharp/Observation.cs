using System;

namespace CSharp
{
    public class Observation
    {
        public string Label { get; private set; }
        public int[] Pixels { get; private set; }

        public Observation(string label, int[] pixels)
        {
            Label = label ?? throw new ArgumentNullException(nameof(label));
            Pixels = pixels ?? throw new ArgumentNullException(nameof(pixels));
        }
    }
}
