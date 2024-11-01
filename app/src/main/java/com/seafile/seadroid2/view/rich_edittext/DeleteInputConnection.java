/*
Copyright 2017 yangchong211（github.com/yangchong211）

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package com.seafile.seadroid2.view.rich_edittext;

import android.view.KeyEvent;
import android.view.inputmethod.InputConnection;
import android.view.inputmethod.InputConnectionWrapper;

import com.seafile.seadroid2.framework.util.SLogs;

/**
 * <pre>
 *     @author 杨充
 *     blog  : https://github.com/yangchong211
 *     time  : 2019/07/18
 *     desc  : 自定义InputConnectionWrapper
 *     revise:
 * </pre>
 */
public class DeleteInputConnection extends InputConnectionWrapper {

    private BackspaceListener mBackspaceListener;

    public interface BackspaceListener {
        /**
         * @return true 代表消费了这个事件
         */
        boolean onBackspace();
    }

    public void setBackspaceListener(BackspaceListener backspaceListener) {
        this.mBackspaceListener = backspaceListener;
    }


    public DeleteInputConnection(InputConnection target, boolean mutable) {
        super(target, mutable);
    }

    /**
     * 提交文本
     * 输入法输入了字符，包括表情，字母、文字、数字和符号等内容，会回调该方法
     *
     * @param text              text
     * @param newCursorPosition 新索引位置
     * @return
     */
    @Override
    public boolean commitText(CharSequence text, int newCursorPosition) {
        return super.commitText(text, newCursorPosition);
    }

    /**
     * 按键事件
     * 在commitText方法中，
     * 如果执行父类的 commitText（即super.commitText(text, newCursorPosition)）那么表示不拦截，
     * 如果返回false则表示拦截
     * <p>
     * 当有按键输入时，该方法会被回调。比如点击退格键时，搜狗输入法应该就是通过调用该方法，
     * 发送keyEvent的，但谷歌输入法却不会调用该方法，而是调用下面的deleteSurroundingText()方法。
     * 网上说少数低端手机，无法响应退格键删除功能，后期找华为p9手机，安装搜狗输入法测试
     *
     * @param event event事件
     * @return
     */
    @Override
    public boolean sendKeyEvent(KeyEvent event) {
        SLogs.d("DeletableEditText---sendKeyEvent--");
        if (event.getKeyCode() == KeyEvent.KEYCODE_DEL && event.getAction() == KeyEvent.ACTION_DOWN) {
            if (mBackspaceListener != null && mBackspaceListener.onBackspace()) {
                return true;
            }
        }
        return super.sendKeyEvent(event);
    }

    /**
     * 删除操作
     * 有文本删除操作时（剪切，点击退格键），会触发该方法
     *
     * @param beforeLength beforeLength
     * @param afterLength  afterLength
     * @return
     */
    @Override
    public boolean deleteSurroundingText(int beforeLength, int afterLength) {
        SLogs.d("DeletableEditText---deleteSurroundingText--" + beforeLength + "----" + afterLength);
        if (beforeLength == 1 && afterLength == 0) {
            return sendKeyEvent(new KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL))
                    && sendKeyEvent(new KeyEvent(KeyEvent.ACTION_UP, KeyEvent.KEYCODE_DEL));
        }
        return super.deleteSurroundingText(beforeLength, afterLength);
    }

    /**
     * 结 束组合文本输入的时候，回调该方法
     *
     * @return
     */
    @Override
    public boolean finishComposingText() {
        return super.finishComposingText();
    }
}
