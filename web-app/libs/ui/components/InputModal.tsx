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
  title?: string;
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

  const t = useTranslation('ui');

  return (
    <Modal layout="vertical" open={Open} onClose={() => onResolve?.(false)} size="xs">
      <Modal.Content size="sm" className="flex flex-col justify-between items-center gap-2">
        <div className="flex justify-end w-full">
          <Icon name="xmark" className="cursor-pointer" onClick={() => onResolve?.(false)} />
        </div>

        <Label size="lg" className="font-medium">
          {title}
        </Label>
        {subtitle && (
          <Label className="text-center text-text-grey" size="xs">
            {subtitle}
          </Label>
        )}

        <InputWrapper required label="Problem name">
          <InputBase onChange={(e) => setName(e.target.value)} value={name}></InputBase>
        </InputWrapper>
        <InputWrapper label="Problem description">
          <Textarea
            onInput={(e) => setDescription(e.currentTarget.value)}
            value={description}
            resizable={false}
          ></Textarea>
        </InputWrapper>

        <div className="!mt-[20px] flex gap-3">
          <Button size="sm" className="w-[150px]" onClick={() => onResolve?.(false)}>
            {t('no')}
          </Button>
          <Button
            outline="primary"
            size="sm"
            className="w-[150px]"
            onClick={() => onResolve?.({ name, description })}
          >
            {t('yes')}
          </Button>
        </div>
      </Modal.Content>
    </Modal>
  );
};
