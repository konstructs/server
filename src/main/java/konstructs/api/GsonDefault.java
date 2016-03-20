package konstructs.api;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

/**
 * GsonDefault is a simple factory class for creating
 * google Gson instances with konstructs defaults.
 *
 * @see  <a href="http://google.github.io/gson/apidocs/com/google/gson/Gson.html">Gson docs</a>
 */
public final class GsonDefault {
    private static Gson defaultGson = getDefaultGsonBuilder().create();

    /**
     * Get a default Gson instance
     * @return The default Gson instance
     */
    public static Gson getDefaultGson() {
        return defaultGson;
    }

    /**
     * Return a default GsonBuilder. This is useful if you need to add
     * anything to the configuration of the Gson instance.
     * @return The default GsonBuilder
     */
    public static GsonBuilder getDefaultGsonBuilder() {
        return new GsonBuilder();
    }

}
