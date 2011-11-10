<?php

class Archiver extends CApplicationComponent {
  
  public $params;
  public $conn;

  public function put($key, $value) {
    if ($this->conn) {
      return dba_replace($key, $value, $this->conn);
    }
    $conn = dba_popen($this->params['filename'], "c", $this->params['handler']);
    dba_replace($key, $value, $conn);
    dba_close($conn);
  }

  public function get($key) {
    Yii::log("Cache miss", "info");
    if ($this->conn) {
      return dba_fetch($key, $this->conn);
    }
    $conn = dba_open($this->params['filename'], "r", $this->params['handler']);
    $value = dba_fetch($key, $conn);
    dba_close($conn);
    return $value;
  }

  public function openPersistent() {
    $this->conn = dba_open($this->params['filename'], 'c', $this->params['handler']);
  }

  public function closePersistent() {
    dba_close($this->conn);
    unset($this->conn);
  }
  
}

