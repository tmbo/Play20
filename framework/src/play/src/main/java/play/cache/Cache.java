package play.cache;

/**
 * Provides an access point for Play's cache service.
 */
public class Cache {

  /**
   * Retrieves an object by key.
   *
   * @return object
   */
  public static Object get(String key) {
    return play.api.cache.Cache.getAsJava(key,play.api.Play.unsafeApplication());
  }

  /**
   * Retrieves an object by key.
   * 
   * @return generic type T
   */
  @SuppressWarnings("unchecked")
  public static <T> T get(String key, Class<T> clazz) {
   return (T)  play.api.cache.Cache.getAsJava(key,play.api.Play.unsafeApplication());
  }

  /**
   * Retrieves an object by keys - provides multi-value access to Play's cache store.
   * 
   * @return a key value list of cache keys and corresponding values
   */
  public static java.util.Map<String,Object> get(String... keys) {
    return play.api.cache.Cache.getAsJava(keys, play.api.Play.unsafeApplication());
  }

  /**
   * Sets a value with expiration.
   * 
   * @param expiration expiration in seconds
   */
  public static void set(String key, Object value, int expiration) {
    play.api.cache.Cache.set(key,value,expiration, play.api.Play.unsafeApplication());
  }

  /**
   * Sets a value, with expiration set to 1800 seconds (30 minutes) by default.
   */
  public static void set(String key, Object value) {
    play.api.cache.Cache.set(key,value, 1800,play.api.Play.unsafeApplication());
  }

}
