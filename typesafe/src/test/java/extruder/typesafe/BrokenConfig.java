package extruder.typesafe;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigList;
import com.typesafe.config.ConfigMemorySize;
import com.typesafe.config.ConfigMergeable;
import com.typesafe.config.ConfigObject;
import com.typesafe.config.ConfigOrigin;
import com.typesafe.config.ConfigResolveOptions;
import com.typesafe.config.ConfigValue;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

public class BrokenConfig implements Config {
  @Override
  public ConfigObject root() {
    return null;
  }

  @Override
  public ConfigOrigin origin() {
    return null;
  }

  @Override
  public Config withFallback(final ConfigMergeable other) {
    return null;
  }

  @Override
  public Config resolve() {
    return null;
  }

  @Override
  public Config resolve(final ConfigResolveOptions options) {
    return null;
  }

  @Override
  public boolean isResolved() {
    return false;
  }

  @Override
  public Config resolveWith(final Config source) {
    return null;
  }

  @Override
  public Config resolveWith(final Config source, final ConfigResolveOptions options) {
    return null;
  }

  @Override
  public void checkValid(final Config reference, final String... restrictToPaths) {

  }

  @Override
  public boolean hasPath(final String path) {
    return false;
  }

  @Override
  public boolean hasPathOrNull(final String path) {
    return false;
  }

  @Override
  public boolean isEmpty() {
    return false;
  }

  @Override
  public Set<Map.Entry<String, ConfigValue>> entrySet() {
    return null;
  }

  @Override
  public boolean getIsNull(final String path) {
    return false;
  }

  @Override
  public boolean getBoolean(final String path) {
    return false;
  }

  @Override
  public Number getNumber(final String path) {
    return null;
  }

  @Override
  public int getInt(final String path) {
    return 0;
  }

  @Override
  public long getLong(final String path) {
    return 0;
  }

  @Override
  public double getDouble(final String path) {
    return 0;
  }

  @Override
  public String getString(final String path) {
    throw new RuntimeException("fail");
  }

  @Override
  public <T extends Enum<T>> T getEnum(final Class<T> enumClass, final String path) {
    return null;
  }

  @Override
  public ConfigObject getObject(final String path) {
    return null;
  }

  @Override
  public Config getConfig(final String path) {
    return null;
  }

  @Override
  public Object getAnyRef(final String path) {
    return null;
  }

  @Override
  public ConfigValue getValue(final String path) {
    return null;
  }

  @Override
  public Long getBytes(final String path) {
    return null;
  }

  @Override
  public ConfigMemorySize getMemorySize(final String path) {
    return null;
  }

  @Override
  public Long getMilliseconds(final String path) {
    return null;
  }

  @Override
  public Long getNanoseconds(final String path) {
    return null;
  }

  @Override
  public long getDuration(final String path, final TimeUnit unit) {
    return 0;
  }

  @Override
  public Duration getDuration(final String path) {
    return null;
  }

  @Override
  public ConfigList getList(final String path) {
    return null;
  }

  @Override
  public List<Boolean> getBooleanList(final String path) {
    return null;
  }

  @Override
  public List<Number> getNumberList(final String path) {
    return null;
  }

  @Override
  public List<Integer> getIntList(final String path) {
    return null;
  }

  @Override
  public List<Long> getLongList(final String path) {
    return null;
  }

  @Override
  public List<Double> getDoubleList(final String path) {
    return null;
  }

  @Override
  public List<String> getStringList(final String path) {
    return null;
  }

  @Override
  public <T extends Enum<T>> List<T> getEnumList(final Class<T> enumClass, final String path) {
    return null;
  }

  @Override
  public List<? extends ConfigObject> getObjectList(final String path) {
    return null;
  }

  @Override
  public List<? extends Config> getConfigList(final String path) {
    return null;
  }

  @Override
  public List<? extends Object> getAnyRefList(final String path) {
    return null;
  }

  @Override
  public List<Long> getBytesList(final String path) {
    return null;
  }

  @Override
  public List<ConfigMemorySize> getMemorySizeList(final String path) {
    return null;
  }

  @Override
  public List<Long> getMillisecondsList(final String path) {
    return null;
  }

  @Override
  public List<Long> getNanosecondsList(final String path) {
    return null;
  }

  @Override
  public List<Long> getDurationList(final String path, final TimeUnit unit) {
    return null;
  }

  @Override
  public List<Duration> getDurationList(final String path) {
    return null;
  }

  @Override
  public Config withOnlyPath(final String path) {
    return null;
  }

  @Override
  public Config withoutPath(final String path) {
    return null;
  }

  @Override
  public Config atPath(final String path) {
    return null;
  }

  @Override
  public Config atKey(final String key) {
    return null;
  }

  @Override
  public Config withValue(final String path, final ConfigValue value) {
    return null;
  }
}
