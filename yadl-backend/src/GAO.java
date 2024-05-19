import java.util.Map;

public class GAO { //General Abstraction Object - No need for templating ;)
    public enum Type {
        Dir,
        Int,
        String,

        None
    }
    public GAO()
    {
        type = Type.None;
    }
    public Type type;
    public String stringData;
    public Integer integerData;

    public Map<String, GAO> directoryData;
}
