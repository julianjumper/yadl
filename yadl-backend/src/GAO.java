import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

public class GAO { //General Abstraction Object - No need for templating ;)
    public enum Type {
        Dir,
        Number,
        String,
        Array,
        Boolean, //Internally a boolean will be saved to numberData (0 or 1)
        None
    }
    public GAO()
    {
        type = Type.None;
    }
    public Type type;
    public String stringData;
    public Double numberData;

    public LinkedList<GAO> arrayData = new LinkedList<>();
    public Map<String, GAO> directoryData = new HashMap<>();
}
