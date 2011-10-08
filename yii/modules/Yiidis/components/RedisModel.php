<?php

class RedisNoObject extends Exception {}
class RedisNotJSON extends Exception {}

class RedisModel extends CFormModel {

  private static $_models = array();

  public static $_keyPrefix = "misc";
  public static $_expire;
  public static $_compress = false;

  public $_key;
  public $_scenario;
  public $_dbAttributes;

  public function __construct($key, $scenario='insert') {
    $this->_key = $key;
    $this->_scenario = $scenario;
    
    $attrs = array();
    foreach($this->attributeNames() as $k) {
      if (substr($k, 0, 1) == '_') continue;
      $attrs[] = $k;
    }

    $this->_dbAttributes = $attrs;   
    parent::__construct();
  }

  public static function unpackJSON($json) {
    $compressed = substr($json, 0, 1);
    if ($compressed == 'c') {
      $json = gzinflate(substr($json, 1));
    }
    return $json;
  }

  public static function fromJSON($key, $json, $skipPrefix = false) {
    $class = get_called_class();
    
    if ($skipPrefix) {
        // gets rid of the prefix
        $key = substr($key, strlen ($class::$_keyPrefix) + 1);
    }
    
    $json = $class::unpackJSON($json);
    $data = json_decode($json, true);

    if ($data === null) {
      throw new RedisNotJSON($key . ' :: ' . $data);
    }

    $obj = new $class($key, 'update');
    $obj->initFromData($data);
    return $obj;
  }

  public static function fromJSONArray($keysAndBlobs, $skipPrefix=false) {
    $class = get_called_class();
    $objs = array();

    for ($i = 0; $i < count($keysAndBlobs); $i += 2) {
      $objs[] = $class::fromJSON($keysAndBlobs[$i], $keysAndBlobs[$i+1], $skipPrefix);
    }

    return $objs;
  }

  public static function fromJSONArrays($keys, $blobs, $skipPrefix=false) {
    $class = get_called_class();
    $objs = array();

    for ($i = 0; $i < count($keys); $i ++) {
      $objs[] = $class::fromJSON($keys[$i], $blobs[$i], $skipPrefix);
    }

    return $objs;
  }

  public static function fromKeyArray($keys, $skipPrefix=false) {
    $class = get_called_class();

    if ($skipPrefix) {
      $getKeys = $keys;
    } else {
      $getKeys = array_map(function($k) use ($class) { return "{$class::$_keyPrefix}:$k"; }, $keys);
    }
    
    $blobs = Yii::app()->redis->conn->mget($getKeys);
    return $class::fromJSONArrays($keys, $blobs);
  }

  public static function getJSON($key, $skipPrefix=false) {
    $class = get_called_class();
    if ($skipPrefix) {
      $fullKey = $key;
    } else {
      $fullKey = $class::$_keyPrefix . ':' . $key;
    }
    $json = Yii::app()->redis->get($fullKey);
    if (!$json) {
      throw new RedisNoObject($key);
    }
    return $json;
  }

  public static function get($key, $skipPrefix=false) {
    $class = get_called_class();
    $json = $class::getJSON($key, $skipPrefix);
    return $class::fromJSON($key, $json, $skipPrefix);
  }

  public static function getArray($key, $skipPrefix=false) {
    $class = get_called_class();
    $json = $class::getJSON($key);
    return  json_decode($class::unpackJSON($json), true);
  }

  public static function ensure($key) {
    $class = get_called_class();
    try {
      return $class::get($key);
    } catch (RedisNoObject $e) {
      return new $class($key);
    }
  }

  public static function model($className=__CLASS__) {
    if (isset(self::$_models[$className])) 
      return self::$_models[$className];

    $model = self::$_models[$className] = new $className(null, null);
    return $model;
  }

  /* -------------------------------------------- */

  public function initFromData($data) {
    foreach ($this->_dbAttributes as $attr) {
      if (isset($data[$attr])) {
        $this[$attr] = $data[$attr];
      }
    }
  }

  public function redisKey() {
    $class = get_called_class();
    return $class::$_keyPrefix . ':' . $this->_key;
  }

  public function put() {
    $dbAttrs = array();
    $attrs = $this->getAttributes();
    foreach ($this->_dbAttributes as $attr) {
      $dbAttrs[$attr] = $attrs[$attr];
    }

    $json = json_encode($dbAttrs, JSON_FORCE_OBJECT);
    $class = get_called_class();

    if ($class::$_compress) {
      $data = 'c' . gzdeflate($json);
      Yii::log("Compressed: " . strlen($json) . " => " . strlen($data), "info");
    } else {
      $data = $json;
    }

    Yii::app()->redis->set($this->redisKey(), 
                           $data, 
                           $this->_scenario);

    if ($class::$_expire) {
      Yii::app()->redis->conn->expire($this->redisKey(), $class::$_expire);
    }
  }

  public function touch() {
    $class = get_called_class();
    if (!$class::$_expire) {
      Yii::app()->redis->conn->persist($this->redisKey());
      return;
    }

    Yii::app()->redis->conn->expire($this->redisKey(), $class::$_expire);
  }

  /* -------------------------------------------- */

  public function transact($fun) {
    function errorException($errno, $errstr, $errfile, $errline) {
      throw new ErrorException($errstr, 0, $errno, $errfile, $errline);
    }
    set_error_handler("errorException");

    try {
      $return = $fun($this);
    } catch(Exception $e) {
      Yii::log("DISCARDING", "info");
      try {
        Yii::app()->redis->conn->discard();
      } catch (Predis\ServerException $_) {}
      restore_error_handler();
      throw $e;
    }

    restore_error_handler();
    return $return;
  }


}

