import org.apache.commons.compress.utils.Lists;
import org.apache.flink.shaded.jackson2.com.fasterxml.jackson.core.JsonParser;
import org.apache.flink.shaded.jackson2.com.fasterxml.jackson.core.JsonProcessingException;
import org.apache.flink.shaded.jackson2.com.fasterxml.jackson.databind.DeserializationContext;
import org.apache.flink.shaded.jackson2.com.fasterxml.jackson.databind.JsonNode;
import org.apache.flink.shaded.jackson2.com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import org.apache.flink.shaded.jackson2.com.fasterxml.jackson.databind.node.IntNode;
import org.apache.flink.shaded.jackson2.com.fasterxml.jackson.databind.node.JsonNodeType;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class CODeserializer extends StdDeserializer<customObject> {

    public CODeserializer() {
        this(null);
    }

    public CODeserializer(Class<?> vc) {
        super(vc);
    }

    private GAO recursiveGAO(JsonNode node, GAO.Type type)
    {
        Iterator<Map.Entry<String, JsonNode>> iterator = node.fields();

        ArrayList<Map.Entry<String, JsonNode>> nodesList = Lists.newArrayList(iterator);
        GAO root = new GAO();
        root.type = type;

        for (Map.Entry<String, JsonNode> nodEntry : nodesList)
        {
            GAO tmp = new GAO();
            String name = nodEntry.getKey();
            JsonNode newNode = nodEntry.getValue();
            JsonNodeType CurType = newNode.getNodeType();
            switch(CurType)
            {
                case ARRAY:
                    tmp = recursiveGAO(newNode, GAO.Type.Array);
                    break;
                case OBJECT:
                    tmp = recursiveGAO(newNode, GAO.Type.Dir);
                    break;
                case NUMBER:
                    tmp.type = GAO.Type.Number;
                    tmp.numberData = newNode.asDouble();
                    break;
                case STRING:
                    tmp.type = GAO.Type.String;
                    tmp.stringData = newNode.asText();
                    break;
                case BOOLEAN:
                    tmp.type = GAO.Type.Boolean;
                    tmp.numberData = (double) (newNode.asBoolean() ? 1:0);
                    break;
                default:
                    tmp.type = GAO.Type.None;
                    break;
            }
           if(root.type == GAO.Type.Array)
           {
               root.arrayData.add(tmp);
           }
           else if(root.type == GAO.Type.Dir)
           {
               root.directoryData.put(name,tmp);
           }
        }
        return root;
    }
    @Override
    public customObject deserialize(JsonParser jp, DeserializationContext ctxt)
            throws IOException, JsonProcessingException {
        JsonNode node = jp.getCodec().readTree(jp);


        customObject obj = new customObject();

        obj.root = recursiveGAO(node, GAO.Type.Dir);

        return obj;
    }
}
