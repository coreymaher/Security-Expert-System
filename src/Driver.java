import jess.*;

public class Driver {

    public static void main(String[] args) {
        Rete engine = new Rete();
        
        try {
            engine.batch("System.clp");
            engine.reset();
            engine.run();
        } catch (JessException e) {
            e.printStackTrace();
        }
    }

}
