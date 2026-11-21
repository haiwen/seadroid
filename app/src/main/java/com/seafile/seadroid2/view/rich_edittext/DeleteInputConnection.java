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
         * @return true if this event was consumed
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
     * Commits text
     * Called when the input method enters characters, including emoji, letters, text, numbers, and symbols
     *
     * @param text              text
     * @param newCursorPosition New cursor position
     * @return
     */
    @Override
    public boolean commitText(CharSequence text, int newCursorPosition) {
        return super.commitText(text, newCursorPosition);
    }

    /**
     * Key events
     * In the commitText method,
     * calling the superclass commitText (i.e. super.commitText(text, newCursorPosition)) means not intercepting,
     * while returning false means intercepting
     * <p>
     * Called when a key is pressed. For example, Sogou input likely calls this method when backspace is pressed
     * to send a keyEvent, whereas Google input calls deleteSurroundingText() below instead.
     * Some low-end phones reportedly do not respond to backspace; test later on a Huawei P9 with Sogou input installed
     *
     * @param event Key event
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
     * Deletion operations
     * Called when text is deleted (by cutting or pressing backspace)
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
     * Called when composing text input is finished
     *
     * @return
     */
    @Override
    public boolean finishComposingText() {
        return super.finishComposingText();
    }
}
