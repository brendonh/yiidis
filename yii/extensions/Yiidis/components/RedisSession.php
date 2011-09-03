<?php

class SessionNoUser extends Exception {}

class RedisSession extends RedisModel {

  public static $_keyPrefix = 'session';
  public static $_expire = 120;

  public $_user;

  public $userID;

  public function getUser() {
    if (!$this->userID) {
      throw new SessionNoUser();
    }

    if (!$this->_user) {
      $this->_user = FacebookUser::get($this->userID);
    }

    return $this->_user;

  }

}