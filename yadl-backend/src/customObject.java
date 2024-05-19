

import org.apache.flink.shaded.jackson2.com.fasterxml.jackson.databind.annotation.JsonDeserialize;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;


@JsonDeserialize(using = CODeserializer.class) public class customObject{
    public HashMap<String, GAO> root;
    public customObject()
    {
        root = new HashMap<String, GAO>();
    }
    public customObject(String key, GAO value)
    {
        root = new HashMap<>();
        root.put(key, value);
    }

    public customObject(customObject obj)
    {
        root = obj.root;
    }

}
