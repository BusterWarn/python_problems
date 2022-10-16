import java.util.StringTokenizer;
import java.io.BufferedReader;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.OutputStream;

class Kattio extends PrintWriter {
    public Kattio(InputStream i) {
        super(new BufferedOutputStream(System.out));
        r = new BufferedReader(new InputStreamReader(i));
    }
    public Kattio(InputStream i, OutputStream o) {
        super(new BufferedOutputStream(o));
        r = new BufferedReader(new InputStreamReader(i));
    }

    public boolean hasMoreTokens() {
        return peekToken() != null;
    }

    public int getInt() {
        return Integer.parseInt(nextToken());
    }

    public double getDouble() {
        return Double.parseDouble(nextToken());
    }

    public long getLong() {
        return Long.parseLong(nextToken());
    }

    public String getWord() {
        return nextToken();
    }



    private BufferedReader r;
    private String line;
    private StringTokenizer st;
    private String token;

    private String peekToken() {
        if (token == null)
            try {
                while (st == null || !st.hasMoreTokens()) {
                    line = r.readLine();
                    if (line == null) return null;
                    st = new StringTokenizer(line);
                }
                token = st.nextToken();
            } catch (IOException e) { }
        return token;
    }

    private String nextToken() {
        String ans = peekToken();
        token = null;
        return ans;
    }
}

public class AddingWords {

    public static void main(String[] args) {

        Kattio io = new Kattio(System.in, System.out);

        String[] defs = new String[2001];
        String s = io.getWord();

        while (io.hasMoreTokens()) {

            if (s.equals("def")) {

                String def = io.getWord();
                int value = io.getInt() + 1000;
                clearWord(defs, def);
                defs[value] = def;

            } else if (s.equals("calc")) {

                int operator = 1; // 1 = + ... -1 = -;
                int sum = 0;
                String word;
                s = io.getWord();

                while (!s.equals("=")) {

                    if (s.equals("+")) {

                        operator = 1;
                    } else if (s.equals("-")) {

                        operator = -1;
                    } else {

                        int value = findValueOfWord(defs, s);
                        if (-1000 > value || 1000 < value) {

                            sum += 100000;
                        } else if (operator > 0) {

                            sum += findValueOfWord(defs, s);
                        } else {

                            sum -= findValueOfWord(defs, s);
                        }
                    }
                    io.print(s + " ");
                    s = io.getWord();
                }
                if (-1000 > sum || 1000 < sum || defs[sum + 1000] == null) {

                    word = "unknown";
                } else {

                    word = defs[sum + 1000];
                }
                io.println("= " + word);

            } else if (s.equals("clear")) {

                clearAllWords(defs);
            }
            s = io.getWord();
        }
        io.flush();
    }

    public static int findValueOfWord(String[] defs, String s) {

        int wordFound = 0;
        int value = 3000;

        for (int i = 0; i < 2001 && wordFound == 0; i++) {

            if (s.equals(defs[i])) {

                wordFound = 1;
                value = i - 1000;
            }
        }
        return value;
    }

    public static void clearWord(String[] defs, String word) {

        int wordDeleted = 0;
        for (int i = 0; i < 2001 && wordDeleted == 0; i++) {

            if (word.equals(defs[i])) {

                defs[i] = null;
                wordDeleted = 1;
            }
        }
    }

    public static void clearAllWords(String[] defs) {

        for (int i = 0; i < 2001; i++) {

            defs[i] = null;
        }
    }
}