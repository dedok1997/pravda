using Expload.Pravda;

[Program]
public class LoosNested
{
    public int TestNestedLoop()
    {
        int a = 0;
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 20; j++) {
	            if (a % 2 == 0) {
                    a += (i + j) % 1000000007;
	            }
	        }
        }

        while (a < 10000) {
            a *= 2;
        }

        return a;
   }

   public static void Main() {}
}
