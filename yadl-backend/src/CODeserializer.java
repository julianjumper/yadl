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
import java.util.Iterator;
import java.util.Map;

public class CODeserializer extends StdDeserializer<customObject> {

    public CODeserializer() {
        this(null);
    }

    public CODeserializer(Class<?> vc) {
        super(vc);
    }

    @Override
    public customObject deserialize(JsonParser jp, DeserializationContext ctxt)
            throws IOException, JsonProcessingException {
        JsonNode node = jp.getCodec().readTree(jp);

        Iterator<Map.Entry<String, JsonNode>> iterator = node.fields();

        ArrayList<Map.Entry<String, JsonNode>> nodesList = Lists.newArrayList(iterator);
        customObject obj = new customObject();
        GAO tmp = new GAO();
        for (Map.Entry<String, JsonNode> nodEntry : nodesList)
        {
            String name = nodEntry.getKey();
            JsonNode newNode = nodEntry.getValue();
            JsonNodeType type = newNode.getNodeType();
            switch(type)
            {
                case ARRAY:
                    break;
                case OBJECT:
                    break;
                case NUMBER:
                    tmp.type = GAO.Type.Int;
                    tmp.integerData = newNode.asInt();
                    break;
                case STRING:
                    tmp.type = GAO.Type.String;
                    tmp.stringData = newNode.asText();
                    break;
            }
            obj.root.put(name,tmp);
        }

        /*else if (node.isArray())
        {
            Iterator<JsonNode> arrayItemsIterator = node.elements();
            ArrayList<JsonNode> arrayItemsList = Lists.newArrayList(arrayItemsIterator);
            for (JsonNode arrayNode : arrayItemsList)
            {
                walker("array item", arrayNode);
            }
        }
        else
        {
            if (node.isValueNode())
            {
                System.out.println("  valueNode: " + node.asText());
            }
            else
            {
                System.out.println("  node some other type");
            }
        }*/

        return obj;
    }
}
