<?php

class FacebookController extends YiidisController {

  public function actions() {
    return array();
  }

  public function actionAfterLogin() {
    Yii::log("Facebook login successful", "info");
    Yii::app()->request->redirect("/");
  }

}