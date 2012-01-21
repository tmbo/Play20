package play.libs;

import java.io.StringWriter;

import org.codehaus.jackson.*;
import org.codehaus.jackson.map.*;
import org.codehaus.jackson.node.*;

/**
 * provides helpers to deal with JSON
 */
 
public class Json {

    /**
     * provides a simple way to serialize into JSON.
     *
     * usage (in a controller):
     *
     * {{{
     * public static Result index() {
     * Map<String,String> d = new HashMap<String,String>();
     * d.put("peter","foo");
     * d.put("yay","value");
     *     return ok(toJson(d));
     * }
     * }}}
     *
     * @param data to be serialized 
     */
    public static JsonNode toJson(final Object data) {
        try {
            return new ObjectMapper().valueToTree(data);
        } catch(Exception e) {
            throw new RuntimeException(e);
        }
    }
   
    public static <A> A fromJson(JsonNode json, Class<A> clazz) {
        try {
            return new ObjectMapper().treeToValue(json, clazz);
        } catch(Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    public static ObjectNode newObject() {
        return new ObjectMapper().createObjectNode();
    }
    
    public static String stringify(JsonNode json) {
        return json.toString();
    }
    
    public static JsonNode parse(String src) {
        try {
            return new ObjectMapper().readValue(src, JsonNode.class);
        } catch(Throwable t) {
            throw new RuntimeException(t);
        }
    }
   
}
