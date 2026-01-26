'use client';

import React from 'react';
import { useTranslation } from '../provider';
import { Button } from './Button';
import { Icon } from './Icon';
import { InputBase } from './InputBase';
import { InputWrapper } from './InputWrapper';
import { Label } from './Label';
import { Modal } from './Modal';
import { Textarea } from './Textarea';

export interface ResolveValue {
  name: string;
  description: string;
}

export interface InputModalProps {
  className?: string;
  onResolve?: (value: ResolveValue | false) => void;
  Open: boolean;
  title?: React.ReactNode;
  subtitle?: string;
  data?: ResolveValue;
}

export const InputModal: React.FC<InputModalProps> = ({
  onResolve,
  data,
  Open,
  title,
  subtitle,
}) => {
  const [name, setName] = React.useState<string>(data?.name ?? '');
  const [description, setDescription] = React.useState<string>(data?.description ?? '');

  const t = useTranslation(data ? 'ui.inputModal.editor.update' : 'ui.inputModal.editor.create');

  return (
    <Modal layout="vertical" open={Open} onClose={() => onResolve?.(false)} size="sm">
      <Modal.Content size="md" className="flex flex-col justify-between items-center gap-2">
        <div className="flex justify-end w-full">
          <Icon name="xmark" className="cursor-pointer" onClick={() => onResolve?.(false)} />
        </div>

        <Label size="lg" className="font-medium">
          {title ?? t('title')}
        </Label>
        {subtitle && <Label className="text-center text-text-grey" size="xs"></Label>}

        <div className="px-10 w-full flex flex-col gap-4">
          <InputWrapper required label="Problem name" className="w-full">
            <InputBase
              onChange={(e) => setName(e.target.value)}
              value={name}
              placeholder={t('placeholderName') as string}
            ></InputBase>
          </InputWrapper>
          <InputWrapper label="Problem description" required={false} className="w-full">
            <Textarea
              size="full"
              onChange={(e) => setDescription(e.target.value)}
              value={description}
              resizable={false}
            ></Textarea>
          </InputWrapper>
        </div>

        <div className="!mt-[20px] flex gap-3">
          <Button
            size="md"
            className="w-[150px]"
            onClick={() => {
              onResolve?.(false);
            }}
          >
            {t('no')}
          </Button>
          <Button
            outline="primary"
            size="md"
            className="w-[150px]"
            onClick={() => {
              onResolve?.({ name, description });
            }}
          >
            {t('yes')}
          </Button>
        </div>
      </Modal.Content>
    </Modal>
  );
};
