<?php

class ObjectExistsException extends Exception {
}


class YiidisClient extends Predis\Client {

  public $trace = false;
  public $count = 0;

  public function __call($method, $arguments) {    
    $this->count++;
    if ($this->trace) {
      $args = CVarDumper::dumpAsString($arguments);
      Yii::log("Redis: {$this->count} : $method : $args", "info");
    }
    return parent::__call($method, $arguments);
  }
}


class RedisConnection extends CApplicationComponent {

  public $params;

  public $conn;

  public $useCache = false;
  public $cache = array();

  public function init() {
    $profile = new Predis\Profiles\ServerVersion24();
    $this->conn = new YiidisClient(
      $this->params,
      array('profile'=>$profile, 'prefix'=>$this->params['prefix']));

    if (isset($this->params['trace']) && $this->params['trace']) $this->conn->trace = true;
    if (isset($this->params['cache']) && $this->params['cache']) $this->useCache = true;

    parent::init();
  }

  public function get($key) {
    if ($this->useCache && isset($this->cache[$key])) {
      return $this->cache[$key];
    }
    $val = $this->conn->get($key);
    if ($this->useCache) $this->cache[$key] = $val;
    return $val;
  }

  public function hget($key, $hashKey) {
    $comboKey = "hash:{$key}--{$hashKey}";
    if ($this->useCache && isset($this->cache[$comboKey])) {
      return $this->cache[$comboKey];
    }
    $val = $this->conn->hget($key, $hashKey);
    if ($this->useCache) $this->cache[$comboKey] = $val;
    return $val;
  }

  public function set($key, $val, $scenario='update') {
    if ($scenario == 'insert') {
      if (!$this->conn->setnx($key, $val))
        throw new ObjectExistsException($key);
    } else {
      if (!$this->conn->set($key, $val))
        // Wrong. What should this throw?
        throw new ObjectExistsException($key);
    }
    
    if ($this->useCache) $this->cache[$key] = $val;

    return true;
  }

}

