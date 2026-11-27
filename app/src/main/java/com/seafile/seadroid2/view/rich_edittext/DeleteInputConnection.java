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
 *     desc  : Custom InputConnectionWrapper
 *     revise:
 * </pre>
 */
public class DeleteInputConnection extends InputConnectionWrapper {

    private BackspaceListener mBackspaceListener;

    public interface BackspaceListener {
        /**
         * @return true if this event is consumed
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
     * Commit text
     * This method is called back when the input method enters characters, including emoji, letters, text, numbers and symbols, etc.
     *
     * @param text              text
     * @param newCursorPosition new cursor position
     * @return
     */
    @Override
    public boolean commitText(CharSequence text, int newCursorPosition) {
        return super.commitText(text, newCursorPosition);
    }

    /**
     * Key event
     * In the commitText method,
     * if the super class commitText is executed (i.e. super.commitText(text, newCursorPosition)), it means no interception,
     * if it returns false, it means interception.
     * <p>
     * This method is called back when there is key input. For example, when the backspace key is tapped, the Sogou input method
     * probably sends a keyEvent through this method, but the Google input method does not call this method; instead it calls
     * the deleteSurroundingText() method below.
     * It is said online that a few low-end phones cannot respond to backspace delete; we later tested by installing the
     * Sogou input method on a Huawei P9 phone.
     *
     * @param event the event
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
     * Delete operation
     * This method is triggered when there is a text delete operation (cut, tap the backspace key).
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
     * This method is called back when composing text input ends.
     *
     * @return
     */
    @Override
    public boolean finishComposingText() {
        return super.finishComposingText();
    }
}
