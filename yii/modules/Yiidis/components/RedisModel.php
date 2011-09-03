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

  public static function fromJSON($key, $json, $skipPrefix = false) {
    $class = get_called_class();
    
    if ($skipPrefix) {
        // gets rid of the prefix
        $key = substr($key, strlen ($class::$_keyPrefix) + 1);
    }
    
    $compressed = substr($json, 0, 1);
    if ($compressed == 'c') {
      $json = gzinflate(substr($json, 1));
    }

    $data = json_decode($json, true);
    if ($data === null) {
      throw new RedisNotJSON($key . ' :: ' . $data);
    }

    $obj = new $class($key, 'update');
    $obj->initFromData($data);
    return $obj;
  }

  public static function get($key, $skipPrefix=false) {
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
    return $class::fromJSON($key, $json, $skipPrefix);
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

}

