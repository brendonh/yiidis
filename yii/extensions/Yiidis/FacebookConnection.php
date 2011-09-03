<?php

class FacebookNeedsLogin extends Exception {}

class FacebookConnection extends CApplicationComponent {

  public $params;
  public $conn;

  public function init() {
    $this->conn = new Facebook($this->params['connection']);
    parent::init();
  }

  public function getUser() {
    $sessionID = Yii::app()->session->sessionID;
    $session = Session::ensure($sessionID);

    if ($session->userID) {
      return $session->getUser();
    }

    $fbUser = $this->conn->getUser();

    if (!$fbUser) {
      throw new FacebookNeedsLogin("No facebook user");
    }

    try {
      $info = $this->conn->api('/me');
      $friends = $this->conn->api('/me/friends');
      $friends = $friends['data'];
    } catch (FacebookApiException $e) {
      throw new FacebookNeedsLogin("No facebook user");
    }

    $userKey = 'fb:' . $info['id'];
    $user = User::ensure($userKey);

    $user->name = $info['name'];
    $user->put();

    $profile = Profile::ensure($userKey);
    $profile->info = $info;
    $profile->friends = $friends;
    $profile->updateFriendCache();
    $profile->put();

    $session->userID = $userKey;
    $session->put();

    return $user;
  }

  public function doLogin() {
    $app = Yii::app();
    $request = $app->getRequest();
    $cb = $request->getBaseUrl() . $app->createUrl("facebook/afterLogin");
    $url = $this->conn->getLoginUrl(array('redirect_uri'=>$cb));
    $request->redirect($url);
  }

}
