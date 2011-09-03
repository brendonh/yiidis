<?php

class FacebookProfile extends RedisModel {

  public static $_keyPrefix = 'profile';
  public static $_compress = true;

  public $info;
  public $friends;

  public function updateFriendCache() {
    $friendIDs = array($this->_key);
    foreach ($this->friends as $fo) {
      $friendIDs[] = 'fb:' . $fo['id'];
    }
    $cacheKey = 'friends:' . $this->_key;
    Yii::app()->redis->conn->del($cacheKey);
    Yii::app()->redis->conn->lpush($cacheKey, $friendIDs);
  }

}