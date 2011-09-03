<?php

class TaskManager {

  public static function addTask($handlerPrefix, $key) {
    $taskKey = "{$handlerPrefix}::{$key}";
    Yii::app()->redis->conn->lpush("queue:queue", $taskKey);
  }

}

