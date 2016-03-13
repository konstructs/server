package konstructs.api;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

public final class GsonDefault {
    private static Gson defaultGson = getDefaultGsonBuilder().create();

    public static Gson getDefaultGson() {
        return defaultGson;
    }

    public static GsonBuilder getDefaultGsonBuilder() {
        return new GsonBuilder();
    }

}
