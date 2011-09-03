<?php

class ObjectExistsException extends Exception {
}

class RedisConnection extends CApplicationComponent {

  public $params;

  public $conn;

  public function init() {
    $profile = new Predis\Profiles\ServerVersion24();
    $this->conn = new Predis\Client(
      $this->params,
      array('profile'=>$profile, 'prefix'=>$this->params['prefix']));
    parent::init();
  }

  public function get($key) {
    return $this->conn->get($key);
  }

  public function set($key, $val, $scenario='update') {
    if ($scenario == 'insert') {
      if (!$this->conn->setnx($key, $val))
        throw new ObjectExistsException($key);
    } else {
      if (!$this->conn->set($key, $val))
        throw new ObjectExistsException($key);
    }

    return true;
  }


}

